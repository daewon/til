name := "lru"

version := "0.1"

sbtVersion := "1.0.2"

scalaVersion := "2.12.3"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.5.3" % "test",
  "org.typelevel" %% "cats-core" % "1.0.0-MF"
)

resolvers ++= Seq(
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

testFrameworks += new TestFramework("utest.runner.Framework")
