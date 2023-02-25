ThisBuild / organization := "com.github.skozlov"
ThisBuild / version := "0.1.0"
ThisBuild / isSnapshot := true
ThisBuild / scalaVersion := "2.13.10"

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Xfatal-warnings",
  "-Xlint",
  "--release:17",
  "-feature",
  "-language:implicitConversions",
)

ThisBuild / coverageEnabled := true
ThisBuild / coverageFailOnMinimum := true
ThisBuild / coverageMinimumStmtTotal := 100

lazy val commonsLang =
  (project in file("commons/lang")).settings(name := "commons-lang")

lazy val commonsTest = (project in file("commons/test")).settings(
  name := "commons-test",
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.15"),
)

lazy val arithmetic = (project in file("arithmetic"))
  .settings(name := "arithmetic")
  .dependsOn(commonsLang, commonsTest % Test)

lazy val algebra = (project in file("algebra"))
  .settings(name := "algebra")
  .dependsOn(arithmetic, commonsTest % Test)

lazy val root = (project in file("."))
  .settings(name := "math")
  .aggregate(commonsLang, commonsTest, arithmetic, algebra)

commands += Command.command("build") { state =>
  "clean" ::
    "scalafmtCheckAll" ::
    "scalafmtSbtCheck" ::
    "coverage" ::
    "test" ::
    "coverageReport" ::
    state
}
