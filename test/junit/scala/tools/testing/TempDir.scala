package scala.tools.testing

import java.io.{IOException, File}
import java.nio.file.{Path, Files}
import scala.util.Properties
import Using.Releasable

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

/* Turn a path into a temp file for purposes of Using it as a resource.
 * On Windows, avoid "file is in use" errors by not attempting to delete it.
 */
case class ForDeletion(path: Path)
object ForDeletion {
  implicit val deleteOnRelease: Releasable[ForDeletion] = new Releasable[ForDeletion] {
    override def release(releasee: ForDeletion) = if (!Properties.isWin) Files.delete(releasee.path)
  }
}
