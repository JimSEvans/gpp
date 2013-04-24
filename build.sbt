name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

retrieveManaged := true

crossPaths := false

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  )

libraryDependencies ++= Seq(
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "org.rogach" %% "scallop" % "0.8.1",
  "org.scalanlp" % "nak" % "1.1.2",
  "com.typesafe.akka" %% "akka-actor" % "2.1.2",
  "commons-codec" % "commons-codec" % "1.7",
  "org.apache.lucene" % "lucene-core" % "4.2.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.0"
)
