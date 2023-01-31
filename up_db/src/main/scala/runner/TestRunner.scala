package runner

import common.types.{SessionId, TestInRepo}
import data.ImplTestsRepo
import db.{jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{TestsMeta, cursor, select_function}
import zio.{ZIO, ZLayer}

import java.sql.{ResultSet, SQLException}
import scala.reflect.internal.ClassfileConstants.instanceof

trait TestRunner {
  def run(sid: SessionId): ZIO[ImplTestsRepo, Throwable, Unit]
}

/**
 * DOCS:
 * https://jdbc.postgresql.org/documentation/callproc/
 * https://postgrespro.com/list/thread-id/1920893
*/
case class TestRunnerImpl(tr: ImplTestsRepo) extends TestRunner {
  import java.sql.Types
  private def exec_select_function_cursor(pgses: pgSess, test: TestInRepo): ZIO[Any, Nothing, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall = ZIO.attemptBlocking{
      //try {
        val tBegin = System.currentTimeMillis
        connection.commit() //todo: remove it !?
        connection.setAutoCommit(false) //todo: remove it !?
        val stmt = connection.prepareStatement(test.call)
        val tExec = System.currentTimeMillis
        val rs: ResultSet = stmt.executeQuery()
        if (!rs.next())
          throw new SQLException("! there were no rows.")

        val v = rs.getObject(1);
        val pgrs = v.asInstanceOf[PgResultSet]
        val columns: IndexedSeq[(String, String)] = (1 to pgrs.getMetaData.getColumnCount)
          .map(cnum => (pgrs.getMetaData.getColumnName(cnum), pgrs.getMetaData.getColumnTypeName(cnum)))
        //todo: in takeWhile we can use condionas from test, rows_eq, rows_gt, rows_lt
        val results: Iterator[IndexedSeq[String]] = Iterator.continually(pgrs).takeWhile(_.next()).map {
          rs => columns.map(cname => rs.getString(cname._1))
        }
        val tFetch = System.currentTimeMillis
        val rowsCnt = results.size
        val res = (tFetch - tBegin, tFetch - tExec, tExec - tBegin, columns, columns.size, rowsCnt)

        rs.close()
        stmt.close()
        res
/*        }
      catch {
    case e: Exception =>
    throw new Exception(e.getMessage)
  }*/
  }.catchAll {
    case e: Exception => ZIO.logError(s" Exception exec_select_function_cursor msg=${e.getMessage} don't fail").as(Unit) //*> //todo: maybe remove here
          //todo: if we onpen it here anr run nultiple test and if one test fail, execution will stoped and error text send to client correctly.
          //todo: use it for whole tests set execution
      //ZIO.fail(throw new Exception(s"Exception for test.id = [${test.id}] with message ${e.getMessage}"))
  }
    res <- execDbCall
    _ <- ZIO.logInfo(s"test res = ${res}")
  } yield ()

  private def exec(sid: SessionId, meta: TestsMeta, test: TestInRepo): ZIO[ImplTestsRepo, Throwable, Unit] = for {
    /*tr <- ZIO.service[ImplTestsRepo]
    tests <- tr.lookup(sid)*/
    jdbc <- jdbcSessionImpl.get.provide(ZLayer.succeed(meta))
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

  def run(sid: SessionId): ZIO[ImplTestsRepo, Throwable, Unit] = for {
      tr <- ZIO.service[ImplTestsRepo]
      testsSetOpt <- tr.lookup(sid)
      _ <- testsSetOpt match {
        case Some(testsSet) =>
          ZIO.logInfo(s" Begin tests set execution for SID = $sid") *>
          ZIO.foreachDiscard(testsSet.tests.getOrElse(List[TestInRepo]()).filter(_.isEnabled == true)) {
            testId: TestInRepo =>
              exec(sid, testsSet.meta, testId)
          }//Here we can use catchAllDefect for catch errors for all tests in set.
        case None => ZIO.unit
      }
    } yield ()
}

  object TestRunnerImpl {
    def get: ZIO[ImplTestsRepo, Throwable, TestRunner] = for {
      tr <- ZIO.service[ImplTestsRepo]
      runner = TestRunnerImpl(tr)
    } yield runner
  }




