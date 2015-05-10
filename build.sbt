name := "functional-programming-with-scala"

version := "0.1.0"

organization := "com.github.mauricio"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.1" % "test")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
