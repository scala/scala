def scalafixVersion = _root_.scalafix.Versions.version
// Use a scala version supported by scalafix.
scalaVersion in ThisBuild := org.scalameta.BuildInfo.supportedScalaVersions.last

lazy val root = project
  .in(file("."))
  .aggregate(rewrites, input, output, tests)

lazy val rewrites = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalafixVersion
)

lazy val input = project.settings(
  scalametaSourceroot := sourceDirectory.in(Compile).value
)

val collections = ProjectRef(file(".."), "collections")

lazy val output = project
  .settings(
    resolvers := resolvers.in(collections).value,
    libraryDependencies := libraryDependencies.in(collections).value,
    scalaVersion := scalaVersion.in(collections).value,
    scalaBinaryVersion := scalaBinaryVersion.in(collections).value
  )
  .dependsOn(collections) // collections/publishLocal is still necessary.
  .disablePlugins(ScalahostSbtPlugin)

lazy val tests = project
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % scalafixVersion % Test cross CrossVersion.full,
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
