import sbt._

import Build._
import Keys._

// This code is adapted from scala.tools.ant.Same by Gilles Dubochet.
object SameTest {

  def checkSameBinaryProjects(lhs: Project, rhs: Project): Project.Initialize[Task[Unit]] =
    (classDirectory in Compile in lhs, classDirectory in Compile in rhs, 
     compile in Compile in lhs, compile in Compile in rhs, streams) map { (lhs,rhs, _, _, s) => 
      // Now we generate a complete set of relative files and then
      def relativeClasses(dir: File) = (dir ** "*.class").get.flatMap(IO.relativize(dir,_).toList)
      // This code adapted from SameTask in the compiler.
      def hasDifferentFiles(filePairs: Seq[(File,File)]): Boolean = {
        filePairs exists { case (a,b) =>
          if (!a.canRead || !b.canRead) {
            s.log.error("Either ["+a+"] or ["+b+"] is missing.")
            true
          } else {
            s.log.debug("Checking for binary differences in ["+a+"] against ["+b+"].")          
            val diff = !checkSingleFilePair(a,b) 
            if(diff) s.log.error("["+a+"] differs from ["+b+"]")
            diff
          }
        }
      }
      val allClassMappings = (relativeClasses(lhs) ++ relativeClasses(rhs)).distinct
      val comparisons = allClassMappings.map(f => new File(lhs, f) -> new File(rhs, f))
      val result = hasDifferentFiles(comparisons)
      if (result) error("Binary artifacts differ.")
    }

  val bufferSize = 1024

  // Tests whether two files are binary equivalents of each other.
  def checkSingleFilePair(originFile: File, destFile: File): Boolean = {
    Using.fileInputStream(originFile) { originStream =>
      Using.fileInputStream(destFile) { destStream =>
        val originBuffer = new Array[Byte](bufferSize)
        val destBuffer = new Array[Byte](bufferSize)
        var equalNow = true
        var originRemaining = originStream.read(originBuffer)
        var destRemaining = destStream.read(destBuffer)
        while (originRemaining > 0 && equalNow) {
          if (originRemaining == destRemaining) {
            for (idx <- 0 until originRemaining) {
              equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))
            }
          } else {
            equalNow = false
          }
          originRemaining = originStream.read(originBuffer)
          destRemaining = destStream.read(destBuffer)
        }
        if (destRemaining > 0) equalNow = false
        equalNow
      }
    }
  }


}
