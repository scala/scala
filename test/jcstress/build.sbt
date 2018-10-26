enablePlugins(JCStressPlugin)

scalaVersion := "2.13.0-pre-SNAPSHOT"

scalaHome := Some(baseDirectory.value / "../../build/pack")
version in Jcstress := "0.4"
resolvers in Global += Resolver.mavenLocal
crossPaths := false

// workaround https://github.com/ktoso/sbt-jcstress/issues/12
internalDependencyClasspath in Test := Nil