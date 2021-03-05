package scala.build

import sbt._, Keys._

/** This object defines keys that should be visible with an unqualified name in all .sbt files and the command line */
object BuildSettings extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val baseVersion = settingKey[String]("The base version number from which all others are derived")
    lazy val baseVersionSuffix = settingKey[String]("Identifies the kind of version to build")
    lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
  }
  import autoImport._

  override def buildSettings = Def.settings(
    ThisBuild / target         := (ThisBuild / baseDirectory).value / "target",
    ThisBuild / buildDirectory := (ThisBuild / baseDirectory).value / "build",
  )
}
