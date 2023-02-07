package runner

import common.types.{SessionId, TestExecutionResult, TestInRepo}
import data.ImplTestsRepo
import db.{jdbcSession, jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{TestsMeta, cursor, select_function}
import zio.{ZIO, ZLayer}

import java.sql.{ResultSet, SQLException}
import scala.reflect.internal.ClassfileConstants.instanceof

trait TestRunner {
  def run(): ZIO[Any, Throwable, Unit]
}

/**
 * DOCS:
 * https://jdbc.postgresql.org/documentation/callproc/
 * https://postgrespro.com/list/thread-id/1920893
*/
case class TestRunnerImpl(tr: ImplTestsRepo, sid: SessionId) extends TestRunner {

  private def updateTestWithResult(test: TestInRepo): ZIO[Any, Throwable, Unit] = for {
    _ <- tr.updateTestWithResults(sid, test)
  } yield ()

  import java.sql.Types
  private def exec_select_function_cursor(pgses: pgSess, test: TestInRepo): ZIO[Any, Throwable, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any,Nothing,TestExecutionResult] = ZIO.attemptBlocking {
      val tBegin = System.currentTimeMillis
      connection.commit() //todo: remove it !?
      connection.setAutoCommit(false) //todo: remove it !?
      val stmt = connection.prepareStatement(test.call)
      val tExec = System.currentTimeMillis
      val rs: ResultSet = stmt.executeQuery()
      val res: TestExecutionResult = {
        if (!rs.next())
        //throw new SQLException("! there were no rows.")
          TestExecutionResult()//0L, 0L, tExec - tBegin, List[(String, String)](), 0)

        val v = rs.getObject(1);
        val pgrs = v.asInstanceOf[PgResultSet]
        val columns: IndexedSeq[(String, String)] = (1 to pgrs.getMetaData.getColumnCount)
          .map(cnum => (pgrs.getMetaData.getColumnName(cnum), pgrs.getMetaData.getColumnTypeName(cnum)))
        // resultsCur.size - internally iterate Iterator to the end.
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
    _ <- updateTestWithResult(test.copy(isExecuted = true, testRes = testResult)/*test.copy(isExecuted = true, testRes = testResult)*/)
  } yield ()


  private def exec(test: TestInRepo): ZIO[TestsMeta with jdbcSession, Throwable, Unit] = for {
    meta <- ZIO.service[TestsMeta]
    jdbc <- ZIO.service[jdbcSession]
    conn <- jdbc.pgConnection
    _ <- ZIO.logInfo(s" ----> sid=[$sid] tests [${test.id}] ${meta.urlMsg} isOpened Connection = ${!conn.sess.isClosed}")
    _ <- test.call_type match {
      case _: select_function.type =>
        test.ret_type match {
          case _: cursor.type => exec_select_function_cursor(conn,test)
          case _ => ZIO.unit
        }
      case _ => ZIO.unit
    }
  } yield ()

  def run: ZIO[Any, Throwable, Unit] = for {
      testsSetOpt <- tr.lookup(sid)
      _ <- testsSetOpt match {
        case Some(testsSet) =>
          ZIO.logInfo(s" Begin tests set execution for SID = $sid") *>
          ZIO.foreachDiscard(testsSet.tests.getOrElse(List[TestInRepo]()).filter(_.isEnabled == true)) {
            testId: TestInRepo =>
              exec(testId).provide(ZLayer.succeed(testsSet.meta), jdbcSessionImpl.layer)
          }//Here we can use catchAllDefect for catch errors for all tests in set.
        case None => ZIO.unit
      }
    } yield ()
}

  object TestRunnerImpl {
    val layer: ZLayer[ImplTestsRepo with SessionId, Throwable, TestRunner] =
      ZLayer.fromFunction((testRepo,sid) => TestRunnerImpl(testRepo,sid))
  }




