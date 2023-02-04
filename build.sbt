organization := "com.github.skozlov"
name := "math"
version := "0.1.0"
isSnapshot := true
scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.14" % Test,
)

scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-Xfatal-warnings",
    "-Xlint",
    "--release:17",
    "-feature", "-language:implicitConversions",
)

coverageEnabled := true
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 100
