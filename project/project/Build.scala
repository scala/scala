import sbt._
object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn(proguard, git)
  lazy val proguard = uri("git://github.com/jsuereth/xsbt-proguard-plugin.git#sbt-0.11")
  lazy val git = uri("git://github.com/sbt/sbt-git-plugin.git#scala-build")
}
