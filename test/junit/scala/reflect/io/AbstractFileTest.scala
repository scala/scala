package scala.reflect.io

import java.nio.file.Files

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.TempDir

@RunWith(classOf[JUnit4])
class AbstractFileTest {
  @Test
  def handleURLEscapedCharacters(): Unit = {
    val tempDir = TempDir.createTempDir().toPath
    val scalaPath = tempDir.resolve("this is a file$.scala")
    Files.createFile(scalaPath)
    val scalaFile = scalaPath.toFile

    try {
      val fileFromURLPath = new java.io.File(scalaFile.toURI.toURL.getPath)
      Assert.assertTrue(!fileFromURLPath.exists())
      val scalacFile = AbstractFile.getURL(scalaFile.toURI.toURL)
      Assert.assertTrue(scalacFile.file.exists())
    } finally {
      Files.deleteIfExists(scalaPath)
      Files.deleteIfExists(tempDir)
    }
  }
}
