def scalafixVersion = _root_.scalafix.Versions.version
inScope(Global)(
  List(
    scalaVersion := _root_.scalafix.Versions.scala212
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(
    rules, input, output, tests,
    `rules-2_13`, `input-2_13`, `output-2_13`, `tests-2_13`
  )

lazy val rules = project.in(file("2.12") / "rules").settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalafixVersion
)

lazy val input = project.in(file("2.12") / "input").settings(
  scalafixSourceroot := sourceDirectory.in(Compile).value
)

val collections = ProjectRef(file(".."), "collectionsJVM")

lazy val output = project.in(file("2.12") / "output")
  .settings(
   resolvers := resolvers.in(collections).value,
   libraryDependencies ++= libraryDependencies.in(collections).value,
   scalaVersion := scalaVersion.in(collections).value,
   scalaBinaryVersion := scalaBinaryVersion.in(collections).value
  )
  .dependsOn(collections) // collections/publishLocal is still necessary.

lazy val tests = project.in(file("2.12") / "tests")
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
  .dependsOn(input, rules)
  .enablePlugins(BuildInfoPlugin)

lazy val `rules-2_13` = project.in(file("2.13") / "rules").settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalafixVersion
)

lazy val `input-2_13` = project.in(file("2.13") / "input")
  .settings(
    scalafixSourceroot := sourceDirectory.in(Compile).value
  )

lazy val `output-2_13` = project.in(file("2.13") / "output")
  .settings(
    resolvers += "scala-pr" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/",
    scalaVersion := "2.13.0-pre-47ffa9c-SNAPSHOT"
  )

lazy val `tests-2_13` = project.in(file("2.13") / "tests")
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % scalafixVersion % Test cross CrossVersion.full,
    buildInfoPackage := "fix",
    buildInfoKeys := Seq[BuildInfoKey](
      "inputSourceroot" ->
        sourceDirectory.in(`input-2_13`, Compile).value,
      "outputSourceroot" ->
        sourceDirectory.in(`output-2_13`, Compile).value,
      "inputClassdirectory" ->
        classDirectory.in(`input-2_13`, Compile).value
    )
  )
  .dependsOn(`input-2_13`, `rules-2_13`)
  .enablePlugins(BuildInfoPlugin)
