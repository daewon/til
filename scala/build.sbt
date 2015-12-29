name := "til"

lazy val commonSettings = Seq(
  organization := "com.kakao.s2graph",
  scalaVersion := "2.11.7",
  version := "0.1",
  scalacOptions := Seq("-language:postfixOps", "-unchecked", "-deprecation", "-feature", "-Xlint"),
  javaOptions ++= collection.JavaConversions.propertiesAsScalaMap(System.getProperties).map{ case (key, value) => "-D" + key + "=" + value }.toSeq,
  testOptions in Test += Tests.Argument("-oDF"),
  parallelExecution in Test := false,
  resolvers ++= Seq(
    Resolver.mavenLocal,
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Cloudera" at "https://repository.cloudera.com/artifactory/cloudera-repos",
    "Twitter Maven" at "http://maven.twttr.com",
    "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"
  )
)

Revolver.settings

lazy val learnToScala = project.settings(commonSettings: _*)

lazy val worksheet = project.settings(commonSettings: _*)

lazy val netty = project.settings(commonSettings: _*)

lazy val rapidoidHttp = project.settings(commonSettings: _*)


