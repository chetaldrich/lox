import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.lox"
ThisBuild / organizationName := "lox"

lazy val root = (project in file("."))
  .settings(
    name := "lox",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.jline" % "jline" % "3.23.0",
  )

