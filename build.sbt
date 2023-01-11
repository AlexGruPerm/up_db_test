name := "up_db_test"

ThisBuild / organization := "yakushev"
ThisBuild / version      := "0.0.1"
ThisBuild / scalaVersion := "2.12.15"

  val Versions = new {
    val zio        = "2.0.5"
    val zio_config = "3.0.7"
    val z_http   = "2.0.0-RC11"
  }

  // PROJECTS
  lazy val global = project
  .in(file("."))
  .settings(commonSettings)
  .disablePlugins(AssemblyPlugin)
  .aggregate(
    up_db
  )

  lazy val up_db = (project in file("up_db"))
  .settings(
    Compile / mainClass        := Some("app.MainApp"),
    assembly / assemblyJarName := "up_db.jar",
    name := "up_db",
    commonSettings,
    libraryDependencies ++= commonDependencies
  )

  lazy val dependencies =
    new {
      val zio = "dev.zio" %% "zio" % Versions.zio

      val zio_conf          = "dev.zio" %% "zio-config"          % Versions.zio_config
      val zio_conf_typesafe = "dev.zio" %% "zio-config-typesafe" % Versions.zio_config
      val zio_conf_magnolia = "dev.zio" %% "zio-config-magnolia" % Versions.zio_config

      val z_http = "io.d11" %% "zhttp" % Versions.z_http

      val zioDep = List(zio, zio_conf,zio_conf_typesafe,zio_conf_magnolia, z_http)
    }

  val commonDependencies = {
    dependencies.zioDep
  }

  lazy val compilerOptions = Seq(
          "-deprecation",
          "-encoding", "utf-8",
          "-explaintypes",
          "-feature",
          "-unchecked",
          "-language:postfixOps",
          "-language:higherKinds",
          "-language:implicitConversions",
          "-Xcheckinit",
          "-Xfatal-warnings",
          "-Ywarn-unused:params,-implicits"
  )

  lazy val commonSettings = Seq(
    scalacOptions ++= compilerOptions,
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      Resolver.DefaultMavenRepository,
      Resolver.mavenLocal,
      Resolver.bintrayRepo("websudos", "oss-releases")
    )++
      Resolver.sonatypeOssRepos("snapshots")
     ++ Resolver.sonatypeOssRepos("public")
     ++ Resolver.sonatypeOssRepos("releases")
  )

  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)

  up_db / assembly / assemblyMergeStrategy := {
    case PathList("module-info.class") => MergeStrategy.discard
    case x if x.endsWith("/module-info.class") => MergeStrategy.discard
    case PathList("META-INF", xs @ _*)         => MergeStrategy.discard
    case "reference.conf" => MergeStrategy.concat
    case _ => MergeStrategy.first
  }