ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode22",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0"
  )
