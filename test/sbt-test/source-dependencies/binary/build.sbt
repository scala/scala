lazy val dep = project.in(file("dep")).settings(
  Compile / packageBin / artifactPath := baseDirectory.value / "target" / s"${name.value}.jar"
)
lazy val use = project.in(file("use"))
