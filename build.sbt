name := "discrete-circuit-simulator"

version := "0.1"

scalaVersion := "2.11.0"

resolvers ++= Seq(
  "snapshots"           at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"            at "http://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions")

javaOptions += ""
