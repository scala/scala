package scala.reflect.io

import java.io.{IOException, File => JFile}
import java.net.{URI, URL}
import java.nio.file.{Files, Path => JPath}
import java.util.jar.{Attributes, Manifest, JarEntry, JarOutputStream}

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.ScalaClassLoader

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


  private def manifestAt(location: URI): URL = ScalaClassLoader.fromURLs(List(location.toURL), null).getResource("META-INF/MANIFEST.MF");

  // ZipArchive.fromManifestURL(URL)
  // was:
  // [error] Test scala.reflect.io.ZipArchiveTest.manifest$u0020resources$u0020just$u0020works failed: java.lang.StackOverflowError: null, took 0.109 sec
  // [error]     at scala.reflect.io.ZipArchive.ensureDir(ZipArchive.scala:113)
  // [error]     at scala.reflect.io.ZipArchive.ensureDir(ZipArchive.scala:115)
  @Test def `manifest resources just works`(): Unit = {
    val jar = createTestJar()
    val archive = new ManifestResources(manifestAt(jar.toUri))
    try {
      val it = archive.iterator
      assertTrue(it.hasNext)
      val f = it.next()
      assertFalse(it.hasNext)
      assertEquals("foo.class", f.name)
    } finally {
      archive.close()
      Files.delete(jar)
    }
  }

  private def createTestJar(): JPath = {
    val f = Files.createTempFile("junit", ".jar")
    val man = new Manifest()
    man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
    man.getEntries().put("foo.class", new Attributes(0))
    val jout = new JarOutputStream(Files.newOutputStream(f), man)
    try {
      jout.putNextEntry(new JarEntry("foo.class"))
      val bytes = "hello, world".getBytes
      jout.write(bytes, 0, bytes.length)
    } finally {
      jout.close()
    }
    f
  }

  @Test def `URL archive works`(): Unit = {
    val z = createTestZip()
    try {
      val url = z.toUri.toURL;
      val zip = new URLZipArchive(url)        // ZipArchive.fromURL(res)
      val zit = zip.iterator
      assertTrue(zit.hasNext)
      val f = zit.next()
      assertFalse(zit.hasNext)
      assertEquals("foo.class", f.name)
    } finally {
      Files.delete(z)
    }
  }

  private def createTestZip(): JPath = {
    import java.util.zip._
    val f = Files.createTempFile("junit", ".zip")
    val zout = new ZipOutputStream(Files.newOutputStream(f))
    try {
      zout.setLevel(Deflater.NO_COMPRESSION)
      zout.setMethod(ZipOutputStream.STORED)
      val entry = new ZipEntry("foo.class")
      val bytes = "hello, world".getBytes
      entry.setSize(bytes.length)
      entry.setCompressedSize(bytes.length)
      entry.setCrc {
        val crc = new CRC32()
        crc.update(bytes, 0, bytes.length)
        crc.getValue
      }
      zout.putNextEntry(entry)
      zout.write(bytes, 0, bytes.length)
      zout.closeEntry()
    } finally {
      zout.close()
    }
    f
  }
}
