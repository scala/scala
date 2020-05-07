package scala.build

import sbt._, Keys._, plugins._

object License extends AutoPlugin {
  val licenseMapping = settingKey[Seq[(File, String)]]("LICENSE/NOTICE file mappings")

  override val requires = JvmPlugin
  override val trigger = AllRequirements

  override def projectSettings: Seq[Def.Setting[_]] =
    List(packageSrc, packageBin, packageDoc)
      .map(task => Compile / task / mappings ++= licenseMapping.value)

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    licenseMapping := List("LICENSE", "NOTICE").map(fn => (baseDirectory.value / fn) -> fn),
  )
}
