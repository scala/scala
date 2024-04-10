package scala.build

import sbt._, Keys._
import scala.util.Properties.isJavaAtLeast

object TestJDeps {
  val testJDepsImpl: Def.Initialize[Task[Unit]] = Def.task {
    val libraryJar = (LocalProject("library") / Compile / packageBin).value
    val reflectJar = (LocalProject("reflect") / Compile / packageBin).value
    val log = streams.value.log
    // in JDK 22, the already-deprecated `-P` option to jdeps was removed,
    // so we can't do the test. it's fine -- it will be a long, long time
    // (probably never) before Scala 2's minimum JVM version is 22+
    if (isJavaAtLeast("22"))
      log.info("can't test jdeps on JDK 22+")
    else {
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
}
