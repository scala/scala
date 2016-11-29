import sbt._

/** This object defines keys that should be visible with an unqualified name in all .sbt files and the command line */
object BuildSettings extends AutoPlugin {
  object autoImport {
    lazy val antStyle = settingKey[Boolean]("Use ant-style incremental builds instead of name-hashing")
    lazy val baseVersion = settingKey[String]("The base version number from which all others are derived")
    lazy val baseVersionSuffix = settingKey[String]("Identifies the kind of version to build")
    lazy val mimaReferenceVersion = settingKey[Option[String]]("Scala version number to run MiMa against")
  }
}
