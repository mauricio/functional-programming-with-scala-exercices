name := "functional-programming-with-scala"

version := "0.1.0"

organization := "com.github.mauricio"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "3.1" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
