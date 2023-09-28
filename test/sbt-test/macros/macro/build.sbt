lazy val `macro-provider` = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
lazy val `macro-client` = project.dependsOn(`macro-provider`)
