name := "netty"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "io.netty" % "netty-all" % "4.1.0.Beta8"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.6" % "test")
