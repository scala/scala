package scala.reflect.io

import java.io.{File => JFile, FileOutputStream, IOException}
import java.util.zip.{ZipEntry, ZipOutputStream}
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

  @Test
  def closeFile: Unit = {
    val f = JFile.createTempFile("test", ".zip")
    val zf = new ZipOutputStream(new FileOutputStream(f))
    zf.putNextEntry(new ZipEntry("data"))
    zf.close()

    val fza = new FileZipArchive(f)

    fza.iterator

    assertTrue(f.canWrite)

    f.delete
  }
}
