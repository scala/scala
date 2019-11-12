package scala.reflect.io

import java.io.IOException
import java.net.{URI, URL}
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{Attributes, Manifest, JarEntry, JarFile, JarOutputStream}
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.ForDeletion
import scala.tools.testkit.Releasables._
import scala.util.chaining._
import scala.util.Using

@RunWith(classOf[JUnit4])
class ZipArchiveTest {

  @Test
  def corruptZip: Unit = {
    val f = Files.createTempFile("test", ".jar")
    val fza = new FileZipArchive(f.toFile)
    try {
      assertThrown[IOException](_.getMessage.contains(f.toString))(fza.iterator)
    } finally {
      Files.delete(f)
    }
  }

  @Test
  def missingFile: Unit = {
    val f = Paths.get("xxx.does.not.exist")
    val fza = new FileZipArchive(f.toFile)
    assertThrown[IOException](_.getMessage.contains(f.toString))(fza.iterator)
  }

  private def manifestAt(location: URI): URL = ScalaClassLoader.fromURLs(List(location.toURL), null).getResource("META-INF/MANIFEST.MF");

  // ZipArchive.fromManifestURL(URL)
  @Test def `manifest resources just works`(): Unit = {
    val jar = createTestJar()
    Using.resources(ForDeletion(jar), new ManifestResources(manifestAt(jar.toUri))) { (_, archive) =>
      val it = archive.iterator
      assertTrue(it.hasNext)
      val f = it.next()
      assertFalse(it.hasNext)
      assertEquals("foo.class", f.name)
    }
  }

  private def createTestJar(): Path = Files.createTempFile("junit", ".jar").tap { f =>
    val man = new Manifest()
    man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
    man.getEntries().put("foo.class", new Attributes(0))
    Using.resource(new JarOutputStream(Files.newOutputStream(f), man)) { jout =>
      jout.putNextEntry(new JarEntry("foo.class"))
      val bytes = "hello, world".getBytes
      jout.write(bytes, 0, bytes.length)
      ()
    }
  }

  // ZipArchive.fromURL(URL)
  @Test def `URL archive works`(): Unit = {
    val z = createTestZip()
    Using.resources(ForDeletion(z), new URLZipArchive(z.toUri.toURL)) { (_, zip) =>
      val zit = zip.iterator
      assertTrue(zit.hasNext)
      val f = zit.next()
      assertFalse(zit.hasNext)
      assertEquals("foo.class", f.name)
    }
  }

  private def createTestZip(): Path = Files.createTempFile("junit", ".zip").tap { f =>
    import java.util.zip._
    Using.resource(new ZipOutputStream(Files.newOutputStream(f))) { zout =>
      zout.setLevel(Deflater.NO_COMPRESSION)
      zout.setMethod(ZipOutputStream.STORED)
      val entry = new ZipEntry("foo.class")
      val bytes = "hello, world".getBytes
      entry.setSize(bytes.length)
      entry.setCompressedSize(bytes.length)
      entry.setCrc(new CRC32().tap(_.update(bytes, 0, bytes.length)).getValue)
      zout.putNextEntry(entry)
      zout.write(bytes, 0, bytes.length)
      zout.closeEntry()
      ()
    }
  }
  /* zipfs doesn't write size field in file header as required by URLZipArchive
  private def createTestZip2(): Path = {
    import java.nio.file.FileSystems
    import java.net.URI
    import scala.util.chaining._
    import scala.jdk.CollectionConverters._
    val f = Files.createTempFile("junit", ".zip")
    Files.delete(f)
    val uri = URI.create(s"jar:${f.toUri}")
    val env = Map("create" -> "true").asJava
    Using.resource(FileSystems.newFileSystem(uri, env)) { fs =>
      val path = fs.getPath("foo.class")
      val bytes = "hello, world".getBytes
      Files.write(path, bytes)
    }
    f.tap(println(_))
  }
   */
}
