package scala.reflect.io

import java.io.{IOException, File => JFile}
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ZipArchiveTest {

  @Test
  def corruptZip {
    val f = JFile.createTempFile("test", ".jar")
    val fza = new FileZipArchive(f)
    try {
      fza.iterator
    } catch {
      case x: IOException =>
        assertTrue(x.getMessage, x.getMessage.contains(f.getPath))
    } finally {
      f.delete()
    }
  }

  @Test
  def missingFile {
    val f = new JFile("xxx.does.not.exist")
    val fza = new FileZipArchive(f)
    try {
      fza.iterator
    } catch {
      case x: IOException =>
        assertTrue(x.getMessage, x.getMessage.contains(f.getPath))
    }
  }
}
