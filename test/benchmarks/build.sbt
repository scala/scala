scalaHome := Some(file("../../build/pack"))

lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "test-benchmarks",
    version := "0.0.1"
  )
