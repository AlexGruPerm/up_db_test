package db

import tmodel.TestsMeta
import zio.{Task, _}

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import java.util.Properties

case class pgSess(sess : Connection, pid : Int)

trait jdbcSession {
  val pgConnection: Task[pgSess]
}

case class jdbcSessionImpl(cp: TestsMeta) extends jdbcSession {
  override val pgConnection: Task[pgSess] = for {
    _ <- ZIO.unit
    sessEffect = ZIO.attemptBlocking{
      try {
        val props = new Properties()
        props.setProperty("user", cp.db_user)
        props.setProperty("password", cp.db_password)
        val c: Connection = DriverManager.getConnection(cp.url, props)
        c.setAutoCommit(false);
        val stmt: Statement = c.createStatement
        val rs: ResultSet = stmt.executeQuery("SELECT pg_backend_pid() as pg_backend_pid")
        rs.next()
        val pg_backend_pid: Int = rs.getInt("pg_backend_pid")
        pgSess(c, pg_backend_pid)
      } catch {
        case e: Exception =>
          throw new Exception(e.getMessage)
      }
    }.catchAll {
      case e: Exception => ZIO.logError(s" Exception jdbcSessionImpl msg=${e.getMessage}") *>
        ZIO.fail(throw new Exception(e.getMessage +cp.urlMsg))
    }
    _ <- ZIO.logInfo(s"  ") *>
      ZIO.logInfo(s"New connection =============== >>>>>>>>>>>>> ")
    sess <- sessEffect
    _ <- ZIO.logInfo(s"pg_backend_pid = ${sess.pid}")
  } yield sess

  def getMaxConnections(connection: pgSess): ZIO[Any,Throwable,Int] =
    for {
      maxConn <- ZIO.attemptBlocking{
        connection.sess.setAutoCommit(false)
        //setting as MAXCONN
        val rs: ResultSet = connection.sess.createStatement.executeQuery(
          """ SELECT setting
            | FROM   pg_settings
            | WHERE  name = 'max_connections' """.stripMargin)
        rs.next()
        rs.getInt("setting")
      }
    } yield maxConn

}

object jdbcSessionImpl {
  def get: ZIO[TestsMeta, Throwable, jdbcSessionImpl] = for {
    tm <- ZIO.service[TestsMeta]
    j = jdbcSessionImpl(tm)
  } yield j
}