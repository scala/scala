package scala.tools.testing

import java.io.{IOException, File}

object TempDir {
  final val TEMP_DIR_ATTEMPTS = 10000
  def createTempDir(): File = {
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val baseName = System.currentTimeMillis() + "-"
    var c = 0
    while (c < TEMP_DIR_ATTEMPTS) {
      val tempDir = new File(baseDir, baseName + c)
      if (tempDir.mkdir()) return tempDir
      c += 1
    }
    throw new IOException(s"Failed to create directory")
  }
}
