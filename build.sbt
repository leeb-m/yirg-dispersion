ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "yirg-dispersion"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.16.0" % "test"
