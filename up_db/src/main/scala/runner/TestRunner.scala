package runner

import common.types.{SessionId, TestExecutionResult, TestInRepo}
import data.ImplTestsRepo
import db.{jdbcSession, jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{TestsMeta, cursor, dataset, func_inout_cursor, select, select_function}
import zio.metrics.{Metric, MetricLabel}
import zio.{UIO, ZIO, ZLayer}

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

  type Columns = IndexedSeq[(String, String)]
  type ListRows = List[IndexedSeq[String]]

  /**
   * Goes through input ResultSet or PgResultSet and return columns with types and rows as List of Seq[String]
  */
  private def columnsRows[A <: ResultSet](rs: A): (Columns,ListRows) ={
    val columns: IndexedSeq[(String, String)] = (1 to rs.getMetaData.getColumnCount)
      .map(cnum => (rs.getMetaData.getColumnName(cnum), rs.getMetaData.getColumnTypeName(cnum)))

    val resultsCur: Iterator[IndexedSeq[String]] = Iterator.continually(rs).takeWhile(_.next()).map {
      rs => columns.map(cname => rs.getString(cname._1))
    }
    val results: List[IndexedSeq[String]] = Iterator.continually(resultsCur).takeWhile(itr => itr.hasNext).flatten.toList
    (columns,results)
  }


  private def exec_func_inout_cursor(pgses: pgSess, test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any,Nothing,TestExecutionResult] = ZIO.attemptBlocking {
      connection.commit()
      connection.setAutoCommit(false)
      val procCallText = s"{call ${test.call} }"
      val stmt = connection.prepareCall(procCallText);
      stmt.setNull(1, Types.OTHER)
      stmt.registerOutParameter(1, Types.OTHER)
      val tBegin = System.currentTimeMillis
      stmt.execute()
      val tExec = System.currentTimeMillis
      val v = stmt.getObject(1)
      val pgrs : PgResultSet = v.asInstanceOf[PgResultSet]

      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        pgrs.close()
        val rowsCnt = rows.size
        TestExecutionResult(tFetch - tBegin, tFetch - tExec, tExec - tBegin, cols, rowsCnt)
      }
      stmt.close()
      res
    }.catchAll {
      case e: Exception => ZIO.logError(s" Exception exec_func_inout_cursor msg=${e.getMessage} don't fail")
        .as(TestExecutionResult(e.getMessage)) //*> //todo: maybe remove here
      //todo: if we onpen it here anr run nultiple test and if one test fail, execution will stoped and error text send to client correctly.
      //todo: use it for whole tests set execution
      //ZIO.fail(throw new Exception(s"Exception for test.id = [${test.id}] with message ${e.getMessage}"))
    }
    testResult <- execDbCall
    _ <- ZIO.logInfo(s"test res = ${testResult}")
    _ <- ZIO.logInfo(s"exec_select_function_cursor rowCount = ${testResult.rowCount}")
    _ <- updateTestWithResult(test.copy(isExecuted = true, testRes = testResult))
  } yield ()

  private def exec_select_dataset(pgses: pgSess, test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any,Nothing,TestExecutionResult] = ZIO.attemptBlocking {
      connection.commit() //todo: remove it !?
      connection.setAutoCommit(false) //todo: remove it !?
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val pgrs: ResultSet = stmt.executeQuery(test.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        pgrs.close()
        val rowsCnt = rows.size
        TestExecutionResult(tFetch - tBegin, tFetch - tExec, tExec - tBegin, cols, rowsCnt)
      }
      stmt.close()
      res
    }.catchAll {
      case e: Exception => ZIO.logError(s" Exception exec_select_dataset msg=${e.getMessage} don't fail")
        .as(TestExecutionResult(e.getMessage)) //*> //todo: maybe remove here
      //todo: if we onpen it here anr run nultiple test and if one test fail, execution will stoped and error text send to client correctly.
      //todo: use it for whole tests set execution
      //ZIO.fail(throw new Exception(s"Exception for test.id = [${test.id}] with message ${e.getMessage}"))
    }
    testResult <- execDbCall
    _ <- ZIO.logInfo(s"test res = ${testResult}")
    _ <- ZIO.logInfo(s"exec_select_dataset rowCount = ${testResult.rowCount}")
    _ <- updateTestWithResult(test.copy(isExecuted = true, testRes = testResult))
  } yield ()

  private def exec_select_function_cursor(pgses: pgSess, test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any,Nothing,TestExecutionResult] = ZIO.attemptBlocking {
      connection.commit() //todo: remove it !?
      connection.setAutoCommit(false) //todo: remove it !?
      val stmt = connection.prepareStatement(test.call)
      val tBegin = System.currentTimeMillis
      val rs: ResultSet = stmt.executeQuery()
      val tExec = System.currentTimeMillis



      val res: TestExecutionResult = {
       if (!rs.next())
          TestExecutionResult()

        val v = rs.getObject(1);
        val pgrs = v.asInstanceOf[PgResultSet]

        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        rs.close()
        val rowsCnt = rows.size
        TestExecutionResult(tFetch - tBegin, tFetch - tExec, tExec - tBegin, cols, rowsCnt)
      }
      stmt.close()
      res
  }.catchAll {
    case e: Exception => ZIO.logError(s" Exception exec_select_function_cursor msg=${e.getMessage} don't fail")
      .as(TestExecutionResult(e.getMessage)) //*> //todo: maybe remove here
          //todo: if we onpen it here anr run nultiple test and if one test fail, execution will stoped and error text send to client correctly.
          //todo: use it for whole tests set execution
      //ZIO.fail(throw new Exception(s"Exception for test.id = [${test.id}] with message ${e.getMessage}"))
  }
    testResult <- execDbCall
    _ <- ZIO.logInfo(s"test res = ${testResult}")
    _ <- ZIO.logInfo(s"exec_select_function_cursor rowCount = ${testResult.rowCount}")
    _ <- updateTestWithResult(test.copy(isExecuted = true, testRes = testResult))
  } yield ()


  private def exec(test: TestInRepo): ZIO[TestsMeta with jdbcSession, Exception, Unit] = for {
    jdbc <- ZIO.service[jdbcSession]
    conn <- jdbc.pgConnection
    _ <- ZIO.logInfo(s" ----> sid=[$sid] tests [${test.id}] isOpened Connection = ${!conn.sess.isClosed}")
    _ <- test.call_type match {
      case _: select_function.type =>
        test.ret_type match {
          case _: cursor.type => exec_select_function_cursor(conn,test) @@
            countAllRequests("select_function_cursor")
          case _ => ZIO.unit
        }
      case _: func_inout_cursor.type =>
        test.ret_type match {
          case _: cursor.type => exec_func_inout_cursor(conn,test) @@
            countAllRequests("func_inout_cursor")
          case _ => ZIO.unit
        }
      case _: select.type =>
        test.ret_type match {
          case _: dataset.type => exec_select_dataset(conn,test) @@
            countAllRequests("select_dataset")
          case _ => ZIO.unit
        }
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
          }//Here we can use catchAllDefect for catch errors for all tests in set.
        case None => ZIO.unit
      }
    } yield ()
}

  object TestRunnerImpl {
    val layer: ZLayer[ImplTestsRepo with SessionId, Exception, TestRunner] =
      ZLayer.fromFunction((testRepo,sid) => TestRunnerImpl(testRepo,sid))
  }




