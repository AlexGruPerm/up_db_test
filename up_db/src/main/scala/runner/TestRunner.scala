package runner

import common.types.{SessionId, TestExecutionResult, TestInRepo}
import data.ImplTestsRepo
import db.{jdbcSession, jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{TestsMeta, cursor, func_inout_cursor, select_function}
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

  private def updateTestWithResult(test: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- tr.updateTestWithResults(sid, test.checkConditions)
  } yield ()

  import java.sql.Types
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
        val columns: IndexedSeq[(String, String)] = (1 to pgrs.getMetaData.getColumnCount)
          .map(cnum => (pgrs.getMetaData.getColumnName(cnum), pgrs.getMetaData.getColumnTypeName(cnum)))
        if (!pgrs.next())
          TestExecutionResult()

        val resultsCur: Iterator[IndexedSeq[String]] = Iterator.continually(pgrs).takeWhile(_.next()).map {
          rs => columns.map(cname => rs.getString(cname._1))
        }
        val results: List[IndexedSeq[String]] = Iterator.continually(resultsCur).takeWhile(itr => itr.hasNext).flatten.toList
        val tFetch = System.currentTimeMillis
        pgrs.close()
        val rowsCnt = results.size
        TestExecutionResult(tFetch - tBegin, tFetch - tExec, tExec - tBegin, columns.toList, rowsCnt)
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


  import java.sql.Types
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
        val columns: IndexedSeq[(String, String)] = (1 to pgrs.getMetaData.getColumnCount)
          .map(cnum => (pgrs.getMetaData.getColumnName(cnum), pgrs.getMetaData.getColumnTypeName(cnum)))

        val resultsCur: Iterator[IndexedSeq[String]] = Iterator.continually(pgrs).takeWhile(_.next()).map {
          rs => columns.map(cname => rs.getString(cname._1))
        }
        val results: List[IndexedSeq[String]] = Iterator.continually(resultsCur).takeWhile(itr => itr.hasNext).flatten.toList
        val tFetch = System.currentTimeMillis
        rs.close()
        val rowsCnt = results.size

/*      todo: If test condition related with data in dataset we can use fetched results as List of rows.
        println(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        results.foreach(println)
        println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")*/
        TestExecutionResult(tFetch - tBegin, tFetch - tExec, tExec - tBegin, columns.toList, rowsCnt)
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
    /*meta <- ZIO.service[TestsMeta]*/
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




