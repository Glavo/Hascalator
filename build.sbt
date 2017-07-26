name := "Hascalator"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.jetbrains" % "annotations" % "13.0"

initialCommands in console := """
    |import Hascalator._
    |import Prelude._
    |""".stripMargin