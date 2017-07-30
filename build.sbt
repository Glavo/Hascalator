name := "Hascalator"

organization := "org.glavo"

version := "0.1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.jetbrains" % "annotations" % "13.0"
)

initialCommands in console := "import Hascalator._, Hascalator.Prelude._"


