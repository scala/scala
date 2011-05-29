seq(ProguardPlugin.proguardSettings :_*)

name := "jline"

organization := "org.scala-lang"

version := "2.10.0-SNAPSHOT"

scalaVersion := "2.9.0-1"

// Only need these because of weird testing jline issues.
retrieveManaged := true

parallelExecution in Test := false

libraryDependencies ++= Seq(
	"org.fusesource.jansi" % "jansi" % "1.4",
	"com.novocode" % "junit-interface" % "0.7" % "test->default"
)

javacOptions ++= Seq("-target", "1.5")

proguardOptions ++= Seq(
  "-dontshrink",
  "-keep class *",
  "-keepdirectories"
)

proguardInJars := Nil

makeInJarFilter ~= { prevFilter =>
  val jansiFilter = List(
    "!META-INF/MANIFEST.MF",
    "org/fusesource/hawtjni/runtime",
    "org/fusesource/hawtjni/runtime/Callback.class",
    "org/fusesource/hawtjni/runtime/Library.class",
    "!org/fusesource/hawtjni/**",
    "!META-INF/maven/org.fusesource.hawtjni",
    "!META-INF/maven/org.fusesource.jansi",
    "!META-INF/maven/org.fusesource.hawtjni/**",
    "!META-INF/maven/org.fusesource.jansi/**"
  ).mkString(",")
  // In sbt 0.9.8 the scala-library.jar line was not necessary,
  // but in 0.9.9 it started showing up here.  Who knows.
  file =>
    if (file startsWith "jansi-") jansiFilter
    else if (file == "scala-library.jar") "!**"
    else prevFilter(file)
}
