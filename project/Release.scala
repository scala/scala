import sbt._
import Keys._

object Release {

  // TODO - Just make the STARR artifacts and dump the sha1 files.

  val starrLibs = Seq("scala-library.jar", "scala-reflect.jar", "scala-compiler.jar", "jline.jar")

  val pushStarr = Command.command("new-starr") { (state: State) =>
    /*val extracted = Project.extract(state)
    import extracted._
    // First run tests
    val (s1, result) = runTask(test in Test, state)
    // If successful, package artifacts
    val (s2, distDir) = runTask(makeExplodedDist, s1)
    // Then copy new libs in place
    val bd = extracted get baseDirectory
    for {
      jarName <- starrLibs
      jar = distDir / "lib" / jarName
      if jar.exists
    } IO.copyFile(jar, bd / "lib" / jarName)
    // Invalidate SHA1 files.
    ShaResolve.removeInvalidShaFiles(bd)
    // Now run tests *again*?
    s2*/
    state
  }
}
