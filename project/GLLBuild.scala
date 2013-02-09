import sbt._
import Keys._

object GLLBuild extends Build {

  lazy val root = 
    Project("Tweak", file("."),
      settings = mainSettings
    ) dependsOn(gllCombinators)
    
  lazy val gllCombinators = 
    uri("https://github.com/jroesch/gll-combinators")
    
  lazy val mainSettings: Seq[Project.Setting[_]] = Defaults.defaultSettings ++ Seq(
    name := "Tweak-Language",
    version := "alpha-1.0",
    scalaVersion := "2.10.0",
    libraryDependencies ++= Seq (
      "org.scala-lang" % "jline" % "2.10.0"
    )
  )
}