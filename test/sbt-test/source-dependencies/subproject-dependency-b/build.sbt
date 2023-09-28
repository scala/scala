lazy val commonSettings = Seq(
  logLevel := Level.Debug
)
lazy val provider = project.settings(commonSettings)
lazy val use = project.settings(commonSettings).dependsOn(provider)
