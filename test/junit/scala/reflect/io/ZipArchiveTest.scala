package scala.reflect.io

import java.io.IOException
import java.net.URL
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{Attributes, Manifest, JarEntry, JarFile, JarOutputStream}
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.testkit.AssertUtil._
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

  @Test def `manifest resources just works`(): Unit = {
    val j = createTestJar();
    try {
      val url = j.toUri.toURL;
      val cl = ScalaClassLoader.fromURLs(List(url), null)
      val res = cl.getResource("META-INF/MANIFEST.MF");
      val arch = new ManifestResources(res)   // ZipArchive.fromManifestURL(res)
      val it = arch.iterator
      assertTrue(it.hasNext)
      val f = it.next()
      assertFalse(it.hasNext)
      assertEquals("foo.class", f.name)
    } finally Files.delete(j)
  }

  private def createTestJar(): Path = {
    val f = Files.createTempFile("junit", ".jar")
    val man = new Manifest()
    man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
    man.getEntries().put("foo.class", new Attributes(0))
    Using.resource(new JarOutputStream(Files.newOutputStream(f), man)) { jout =>
      jout.putNextEntry(new JarEntry("foo.class"))
      val bytes = "hello, world".getBytes
      jout.write(bytes, 0, bytes.length)
      jout.close()
      f
    }
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
    } finally Files.delete(z)
  }

  private def createTestZip(): Path = {
    import java.util.zip._
    import scala.util.chaining._
    val f = Files.createTempFile("junit", ".zip")
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
      f
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
