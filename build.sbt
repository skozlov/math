organization := "com.github.skozlov"
name := "math"
version := "0.1.0"
isSnapshot := true
scalaVersion := "2.13.10"

scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-Xfatal-warnings",
    "-Xlint",
    "--release:17",
)

coverageEnabled := true
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 100
