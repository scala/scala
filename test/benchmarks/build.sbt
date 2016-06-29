scalaHome := Some(file("../../build/pack"))
scalaVersion := "2.12.0-dev"
scalacOptions ++= Seq("-feature", "-opt:l:classpath")

lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "test-benchmarks",
    version := "0.0.1",
	libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.4"
  )
