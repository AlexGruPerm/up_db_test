package runner

import common.{CallTimings, TestExecutionResult, TestInRepo}
import common.types.SessionId
import data.ImplTestsRepo
import db.{jdbcSession, jdbcSessionImpl, pgSess}
import org.postgresql.jdbc.PgResultSet
import tmodel.{TestsMeta, affected_rows, cursor, dataset, dml_sql, func_inout_cursor, integer_value, select, select_function}
import zio.metrics.{Metric, MetricLabel}
import zio.{ UIO, ZIO, ZLayer}
import common.types._
import java.sql.{Connection, ResultSet}

  trait TestRunner {
    def run(): ZIO[Any, Exception, Unit]
  }

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
    val results: ListRows = Iterator.continually(resultsCur).takeWhile(itr => itr.hasNext).flatten.toList
    rs.close()
    (columns,results)
  }

  private def execCallUpdateTestInRepo(dbCall: UIO[TestExecutionResult], test: TestInRepo): ZIO[Any,Exception,Unit] = for {
    testResult <- dbCall
    _ <- updateTestWithResult(test.copy(
      isExecuted = true,
      testRes = testResult,
      countOfExecuted = test.countOfExecuted+1))
  } yield ()

  private def makeCommit(use: Option[Boolean], conn: Connection) =
    if (use.getOrElse(false))
      conn.commit()

  private def exec_func_inout_cursor(pgses: pgSess, testInRepo: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val procCallText = s"{call ${testInRepo.test.call} }"
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
      makeCommit(testInRepo.test.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_select_dataset(pgses: pgSess, testInRepo: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val pgrs: ResultSet = stmt.executeQuery(testInRepo.test.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      makeCommit(testInRepo.test.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_select_function_int(pgses: pgSess, testInRepo: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val pgrs: ResultSet = stmt.executeQuery(testInRepo.test.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = columnsRows(pgrs)
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rows.size)
      }
      makeCommit(testInRepo.test.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_dml_sql(pgses: pgSess, testInRepo: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.createStatement()
      val tBegin = System.currentTimeMillis
      val rowsAffected: Int = stmt.executeUpdate(testInRepo.test.call);
      val tExec = System.currentTimeMillis
      val res: TestExecutionResult = {
        val (cols: Columns, rows: ListRows) = (IndexedSeq[Column](),List[IndexedSeq[String]]())
        val tFetch = System.currentTimeMillis
        TestExecutionResult(CallTimings(tBegin,tExec,tFetch), cols, rowsAffected)
      }
      makeCommit(testInRepo.test.use_commit, connection)
      stmt.close()
      connection.close()
      res
    }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def exec_select_function_cursor(pgses: pgSess, testInRepo: TestInRepo): ZIO[Any, Exception, Unit] = for {
    _ <- ZIO.unit
    connection = pgses.sess
    execDbCall: ZIO[Any, Throwable, TestExecutionResult] = ZIO.attemptBlocking {
      val stmt = connection.prepareStatement(testInRepo.test.call)
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
      makeCommit(testInRepo.test.use_commit, connection)
      stmt.close()
      connection.close()
      res
  }
    execDbCallCatched = catchAllErrs(execDbCall)
    _ <- execCallUpdateTestInRepo(execDbCallCatched,testInRepo)
  } yield ()

  private def catchAllErrs(eff: ZIO[Any, Throwable, TestExecutionResult]): UIO[TestExecutionResult] = for {
    effAfterCatch <- eff.catchAllDefect {
      case e: Exception => ZIO.logError(e.getMessage).as(TestExecutionResult(e.getClass.getName,e.getMessage))
    }.catchAll {
      e: Throwable => ZIO.logError(e.getMessage).as(TestExecutionResult(e.getClass.getName,e.getMessage))
    }
  } yield effAfterCatch

  private def exec(testInRepo: TestInRepo): ZIO[TestsMeta with jdbcSession, Exception, Unit] = for {
    jdbc <- ZIO.service[jdbcSession]
    conn <- jdbc.pgConnection(testInRepo.test.id)
    _ <- ZIO.logInfo(s" ----> sid=[$sid] tests [${testInRepo.test.id}] isOpened Connection = ${!conn.sess.isClosed}")
    _ <- (testInRepo.test.call_type,testInRepo.test.ret_type) match {
      case (_: select_function.type, _: cursor.type) => exec_select_function_cursor(conn,testInRepo) @@
        countAllRequests("select_function_cursor")
      case (_: select_function.type, _: integer_value.type) => exec_select_function_int(conn,testInRepo) @@
        countAllRequests("select_function_integer_value")
      case (_: func_inout_cursor.type , _: cursor.type) => exec_func_inout_cursor(conn,testInRepo) @@
        countAllRequests("func_inout_cursor")
      case (_: select.type, _: dataset.type) => exec_select_dataset(conn,testInRepo) @@
        countAllRequests("select_dataset")
      case (_: dml_sql.type, _: affected_rows.type) => exec_dml_sql(conn,testInRepo) @@
        countAllRequests("dml_sql")
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

  def run(): ZIO[Any, Exception, Unit] = for {
      testsSetOpt <- tr.lookup(sid)
      _ <- testsSetOpt match {
        case Some(testsSet) =>
          ZIO.logInfo(s" Begin tests set execution for SID = $sid") *>
          ZIO.foreachDiscard(testsSet.optListTestInRepo.getOrElse(List[TestInRepo]()).filter(_.test.isEnabled == true)) {
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




