import sbt._
import Keys._

object GLLBuild extends Build {
  lazy val tweak = Project("Tweak", file("."),
    settings = Defaults.defaultSettings
  ) dependsOn(gllCombinators)

  lazy val gllCombinators = uri("https://github.com/jroesch/gll-combinators")
}