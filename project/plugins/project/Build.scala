import sbt._
object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn(proguard)
  lazy val proguard = uri("git://github.com/jsuereth/xsbt-proguard-plugin.git")
  //lazy val proguard = uri("git://github.com/siasia/xsbt-proguard-plugin.git")
}
