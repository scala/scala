// Use a scala version supported by scalafix.
scalaVersion in ThisBuild := org.scalameta.BuildInfo.supportedScalaVersions.last

lazy val root = project.aggregate(rewrites, input, output, tests)

lazy val rewrites = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % "0.4.2"
)

lazy val input = project.settings(
  scalametaSourceroot := sourceDirectory.in(Compile).value
)

lazy val output = project

lazy val tests = project
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % "0.4.2" % Test cross CrossVersion.full,
    buildInfoPackage := "fix",
    buildInfoKeys := Seq[BuildInfoKey](
      "inputSourceroot" ->
        sourceDirectory.in(input, Compile).value,
      "outputSourceroot" ->
        sourceDirectory.in(output, Compile).value,
      "inputClassdirectory" ->
        classDirectory.in(input, Compile).value
    )
  )
  .dependsOn(input, rewrites)
  .enablePlugins(BuildInfoPlugin)
