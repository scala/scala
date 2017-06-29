scalaHome := Some(file("../../build/pack"))
scalaVersion := "2.12.1-dev"
scalacOptions ++= Seq("-feature", "-opt:l:inline", "-opt-inline-from:**")

lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "test-benchmarks",
    version := "0.0.1",
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.6"
  )
