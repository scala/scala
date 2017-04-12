addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.20")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.0-RC2")

// for bencharts
libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.0.14",
  "com.typesafe.play" %% "play-json" % "2.4.10"
)
