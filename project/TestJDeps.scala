package scala.build

import sbt._, Keys._

object TestJDeps {
  val testJDepsImpl: Def.Initialize[Task[Unit]] = Def.task {
    val libraryJar = (LocalProject("library") / Compile / packageBin).value
    val reflectJar = (LocalProject("reflect") / Compile / packageBin).value

    // jdeps -s -P build/pack/lib/scala-{library,reflect}.jar | grep -v build/pack | perl -pe 's/.*\((.*)\)$/$1/' | sort -u
    val jdepsOut = scala.sys.process.Process("jdeps", Seq("-s", "-P", libraryJar.getPath, reflectJar.getPath)).lineStream

    val profilePart = ".*\\((.*)\\)$".r
    val profiles = jdepsOut.collect {
      case profilePart(profile) => profile
    }.toSet

    if (profiles != Set("compact1"))
      throw new RuntimeException(jdepsOut.mkString("Detected dependency outside of compact1:\n", "\n", ""))
  }
}
