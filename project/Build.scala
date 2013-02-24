import sbt._
import Keys._

object GLLBuild extends Build {

  lazy val root = 
    Project("root", file("."),
      settings = mainSettings
    ) dependsOn(gllCombinators)
  /* make sure it is git:// */
  lazy val gllCombinators = 
    uri("git://github.com/djspiewak/gll-combinators.git")
    
  lazy val mainSettings: Seq[Project.Setting[_]] = Project.defaultSettings ++ Seq(
    name := "Tweak-Language",
    version := "alpha-1.0",
    scalaVersion := "2.10.0",
    libraryDependencies ++= Seq (
      "org.scala-lang" % "jline" % "2.10.0",
      "org.specs2" %% "specs2" % "1.14" % "test",
      "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
    ),
    resolvers ++= Seq(
      "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "releases"  at "http://oss.sonatype.org/content/repositories/releases"
    ),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )
}