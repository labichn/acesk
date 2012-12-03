import sbt._
import sbt.Keys._

object ACESKBuild extends Build {
  import Dependencies._
  import Settings._

  lazy val acesk = Project(
    id = "acesk",
    base = file("."),
    settings = buildSettings ++ Seq(libraryDependencies ++= commonDependencies)
  )

}

object Resolvers {

  lazy val ivyLocalRepo = "Ivy Local" at "file://"+Path.userHome+"/.ivy2/local"
  lazy val sonatypeRepo = "Sonatype OSS Repo" at "https://oss.sonatype.org/content/groups/public/"

  lazy val repos = Seq(sonatypeRepo, ivyLocalRepo)

}

object Dependencies {

  private lazy val shared = Seq(scalaTest)
  lazy val commonDependencies = shared

  lazy val scalaTestVersion = "1.8"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

}

object Settings {
  import Dependencies._
  import Resolvers._

  lazy val projectName = "acesk"

  lazy val buildOrg = "labichn"
  lazy val buildVersion = "0.0.1"
  lazy val buildScalaVersion = "2.9.2"

  lazy val compilerArgs = Seq("-deprecation", "-unchecked")

  lazy val buildSettings = Defaults.defaultSettings ++
    Seq(organization := buildOrg,
        version := buildVersion,
        scalaVersion := buildScalaVersion) ++
    Seq(resolvers ++= repos,
        scalacOptions ++= compilerArgs)
}
