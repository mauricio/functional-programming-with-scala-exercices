name := "functional-programming-with-scala"

version := "0.1.0"

organization := "com.github.mauricio"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")