package runner

import common.types.{SessionId, TestExecutionResult, TestInRepo}
import data.ImplTestsRepo
import db.{jdbcSession, jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{TestsMeta, cursor, dataset, func_inout_cursor, select, select_function}
import zio.metrics.{Metric, MetricLabel}
import zio.{Task, UIO, ZIO, ZLayer}
import common.types._

import java.sql.{ResultSet, SQLException}
import scala.reflect.internal.ClassfileConstants.instanceof

  trait TestRunner {
    def run(): ZIO[Any, Exception, Unit]
  }

/**
 * DOCS:
 * https://jdbc.postgresql.org/documentation/callproc/
 * https://postgrespro.com/list/thread-id/1920893
*/
  case class TestRunnerImpl(tr: ImplTestsRepo, sid: SessionId) extends TestRunner {
  import java.sql.Types
  private def updateTestWithResult(test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- tr.updateTestWithResults(sid, test.checkConditions)
  } yield ()


  /**
   * Goes through input ResultSet or PgResultSet and return columns with types and rows as List of Seq[String]
  */
  private def columnsRows[A <: ResultSet](rs: A): (Columns,ListRows) ={
    val columns: Columns = (1 to rs.getMetaData.getColumnCount)
      .map(cnum => (rs.getMetaData.getColumnName(cnum), rs.getMetaData.getColumnTypeName(cnum)))
    val resultsCur: Iterator[IndexedSeq[String]] = Iterator.continually(rs).takeWhile(_.next()).map {
      rs => columns.map(cname => rs.getString(cname._1))
    }
    val results: List[IndexedSeq[String]] = Iterator.continually(resultsCur).takeWhile(itr => itr.hasNext).flatten.toList
    rs.close()
    (columns,results)
  }

  private def execCallUpdateTestInRepo(dbCall: UIO[TestExecutionResult], test: TestInRepo): ZIO[Any,Exception,Unit] = for {
    testResult <- dbCall
    _ <- updateTestWithResult(test.copy(isExecuted = true, testRes = testResult))
  } yield ()

  private def exec_func_inout_cursor(pgses: pgSess, test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val procCallText = s"{call ${test.call} }"
      val stmt = connection.prepareCall(procCallText);
      stmt.setNull(1, Types.OTHER)
      stmt.registerOutParameter(1, Types.OTHER)
      val tBegin = System.currentTimeMillis
      stmt.execute()
      val tExec = System.currentTimeMillis
      val pgrs = stmt.getObject(1).asInstanceOf[PgResultSet]
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      stmt.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,test)
  } yield ()

  private def exec_select_dataset(pgses: pgSess, test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val pgrs: ResultSet = stmt.executeQuery(test.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      stmt.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,test)
  } yield ()

  private def exec_select_function_cursor(pgses: pgSess, test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.prepareStatement(test.call)
      val tBegin = System.currentTimeMillis
      val rs: ResultSet = stmt.executeQuery()
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
       if (!rs.next())
          TestExecutionResult()
        val pgrs = rs.getObject(1).asInstanceOf[PgResultSet]
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      stmt.close()
      res
  }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,test)
  } yield ()

  private def catchAllErrs(eff: ZIO[Any, Throwable, TestExecutionResult]): UIO[TestExecutionResult] = for {
    effAfterCatch <- eff.catchAllDefect {
      case e: Exception => ZIO.logError(e.getMessage).as(TestExecutionResult(e.getMessage))
    }.catchAll {
      e: Throwable => ZIO.logError(e.getMessage).as(TestExecutionResult(e.getMessage))
    }
  } yield effAfterCatch

  private def exec(test: TestInRepo): ZIO[TestsMeta with jdbcSession, Exception, Unit] = for {
    jdbc <- ZIO.service[jdbcSession]
    conn <- jdbc.pgConnection
    _ <- ZIO.logInfo(s" ----> sid=[$sid] tests [${test.id}] isOpened Connection = ${!conn.sess.isClosed}")
    //todo: convert switches in case of comparing pairs (test.call_type,test.ret_type)  match { case (_:A,_:B) => ... }
    _ <- (test.call_type,test.ret_type) match {
      case (_: select_function.type, _: cursor.type) => exec_select_function_cursor(conn,test) @@
        countAllRequests("select_function_cursor")
      case (_: func_inout_cursor.type , _: cursor.type) => exec_func_inout_cursor(conn,test) @@
        countAllRequests("func_inout_cursor")
      case (_: select.type, _: dataset.type) => exec_select_dataset(conn,test) @@
        countAllRequests("select_dataset")
      case _ => ZIO.unit
    }
  } yield ()

  private val execInRunCount = Metric.counterInt("call_exec_in_run").fromConst(1)

  def getExecInRunCount: UIO[Double] = for {
    c <- execInRunCount.value
  } yield c.count

  def countAllRequests(method: String) =
    Metric.counterInt("count_all_exec").fromConst(1)
      .tagged(
        MetricLabel("method", method)
      )

  def run: ZIO[Any, Exception, Unit] = for {
      testsSetOpt <- tr.lookup(sid)
      _ <- testsSetOpt match {
        case Some(testsSet) =>
          ZIO.logInfo(s" Begin tests set execution for SID = $sid") *>
          ZIO.foreachDiscard(testsSet.tests.getOrElse(List[TestInRepo]()).filter(_.isEnabled == true)) {
            testId: TestInRepo =>
              exec(testId).provide(ZLayer.succeed(testsSet.meta), jdbcSessionImpl.layer) @@ execInRunCount
          }
        case None => ZIO.unit
      }
    } yield ()
}

  object TestRunnerImpl {
    val layer: ZLayer[ImplTestsRepo with SessionId, Exception, TestRunner] =
      ZLayer.fromFunction((testRepo,sid) => TestRunnerImpl(testRepo,sid))
  }




