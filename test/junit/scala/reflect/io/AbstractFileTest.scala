package scala.reflect.io

import org.junit.jupiter.api.{Assertions, Test}

import java.nio.file.Files
import scala.tools.testkit.TempDir

class AbstractFileTest {
  @Test
  def handleURLEscapedCharacters(): Unit = {
    val tempDir = TempDir.createTempDir().toPath
    val scalaPath = tempDir.resolve("this is a file$.scala")
    Files.createFile(scalaPath)
    val scalaFile = scalaPath.toFile

    try {
      val fileFromURLPath = new java.io.File(scalaFile.toURI.toURL.getPath)
      Assertions.assertTrue(!fileFromURLPath.exists())
      val scalacFile = AbstractFile.getURL(scalaFile.toURI.toURL)
      Assertions.assertTrue(scalacFile.file.exists())
    } finally {
      Files.deleteIfExists(scalaPath)
      Files.deleteIfExists(tempDir)
    }
  }
}
