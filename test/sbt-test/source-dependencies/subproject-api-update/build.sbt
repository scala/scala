//ThisBuild / logLevel         := Level.Debug

lazy val core = project
lazy val root = project.dependsOn(core)

// not in common.sbt.template because analysis is taken from `root` subproject
InputKey[Unit]("checkNameExistsInClass") := {
  val className :: name :: Nil = complete.DefaultParsers.spaceDelimited("<arg>").parsed
  val analysis = (root / Compile / compile).value.asInstanceOf[sbt.internal.inc.Analysis]
  assert(analysis.apis.externalAPI(className).nameHashes.exists(_.name == name))
}
