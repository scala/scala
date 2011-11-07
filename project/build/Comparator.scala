import sbt._
import java.io.{File, FileInputStream}

// Based on scala.tools.ant.Same
object Comparator {

    private def getMappedPath(path: Path, baseDirectory: Path): Path = {
      Path.fromString(baseDirectory, path.relativePath)
    }


    def compare(origin: Path, dest: Path, filter: Path => PathFinder, log: Logger): Option[String] = {
      log.info("Comparing the contents of "+origin.absolutePath+ " with "+dest.absolutePath)
      var allEqualNow = true

      def reportDiff(f1: File, f2: File) = {
        allEqualNow = false
        log.error("File '" + f1 + "' is different from correspondant.")
      }

      def reportMissing(f1: File) = {
        allEqualNow = false
        log.error("File '" + f1 + "' has no correspondant.")
      }



      val originPaths = filter(origin).get
      
      val bufferSize = 1024
      val originBuffer = new Array[Byte](bufferSize)
      val destBuffer = new Array[Byte](bufferSize)
      
      for (originPath <- originPaths.filter(! _.isDirectory)){
        log.debug("origin :" + originPath.absolutePath)
        val destPath = getMappedPath(originPath, dest)
        log.debug("dest   :" + destPath.absolutePath)
        var equalNow = true      
        val originFile = originPath.asFile
        val destFile = destPath.asFile

        if (originFile.canRead && destFile.canRead) {
        
        val originStream = new FileInputStream(originFile)
        val destStream = new FileInputStream(destFile)
        var originRemaining = originStream.read(originBuffer)
        var destRemaining = destStream.read(destBuffer)
        while (originRemaining > 0 && equalNow) {
          if (originRemaining == destRemaining)
            for (idx <- 0 until originRemaining) {
              equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))}
          else
            equalNow = false
          originRemaining = originStream.read(originBuffer)
          destRemaining = destStream.read(destBuffer)
        }
        if (destRemaining > 0) equalNow = false

        if (!equalNow) reportDiff(originFile, destFile)

        originStream.close
        destStream.close

      }
      else reportMissing(originFile)
       
      }
      if(allEqualNow) None else Some("There were differences between "+origin.absolutePath+ " and "+ dest.absolutePath)
    }


}
