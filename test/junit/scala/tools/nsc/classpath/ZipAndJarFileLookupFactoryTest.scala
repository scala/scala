package scala.tools.nsc
package classpath

import org.junit.Assert._
import org.junit.Test
import java.net.{URI, URL}
import java.nio.file._
import java.nio.file.attribute.FileTime
import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.io.AbstractFile
import scala.tools.testkit.ForDeletion
import scala.tools.testkit.Releasables._
import scala.util.chaining._
import scala.util.Using

class ZipAndJarFileLookupFactoryTest {
  @Test def cacheInvalidation(): Unit = {
    if (scala.util.Properties.isWin) return // can't overwrite an open file on windows.

    val f = Files.createTempFile("test-", ".jar")
    Files.delete(f)
    val g = new Global(new Settings())
    assert(!g.settings.YdisableFlatCpCaching.value) // we're testing with our JAR metadata caching enabled.
    Using.resources(new CloseableRegistry, ForDeletion(f)) { (closeableRegistry, _) =>
      def createCp = ZipAndJarClassPathFactory.create(AbstractFile.getFile(f.toFile), g.settings, closeableRegistry)
      createZip(f, Array(), "p1/C.class")
      createZip(f, Array(), "p2/X.class")
      createZip(f, Array(), "p3/Y.class")
      val cp1 = createCp
      assert(cp1.findClass("p1.C").isDefined)

      // We expect get a cache hit as the underlying zip hasn't changed
      val cp2 = createCp
      assert(cp2 eq cp1)

      // check things work after the cache hit
      cp1.findClassFile("p2.X").get.toByteArray

      val lastMod1 = Files.getLastModifiedTime(f)
      // Create a new zip at the same path with different contents and last modified
      Files.delete(f)
      createZip(f, Array(), "p1/D.class")
      Files.setLastModifiedTime(f, FileTime.fromMillis(lastMod1.toMillis + 2000))

      // Our classpath cache should create a new instance
      val cp3 = createCp
      assert(cp1 ne cp3, (System.identityHashCode(cp1), System.identityHashCode(cp3)))
      // And that instance should see D, not C, in package p1.
      assert(cp3.findClass("p1.C").isEmpty)
      assert(cp3.findClass("p1.D").isDefined)
    }
  }

  private def createZip(zipLocation: Path, content: Array[Byte], internalPath: String): Unit = {
    val env = new java.util.HashMap[String, String]()
    env.put("create", String.valueOf(Files.notExists(zipLocation)))
    val fileUri = zipLocation.toUri
    val zipUri = new java.net.URI("jar:" + fileUri.getScheme, fileUri.getPath, null)
    Using.resource(FileSystems.newFileSystem(zipUri, env)) { zipfs =>
      val internalTargetPath = zipfs.getPath(internalPath)
      Files.createDirectories(internalTargetPath.getParent)
      Files.write(internalTargetPath, content)
    }
  }

  // ZipArchive.fromManifestURL(URL)
  @Test def `manifest classpath entry works`(): Unit = {
    import java.util.jar.{Attributes, Manifest, JarEntry, JarOutputStream}
    import scala.reflect.io.ManifestResources

    def createTestJar(): Path = Files.createTempFile("junit", ".jar").tap { f =>
      val man = new Manifest()
      man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
      man.getEntries().put("foo.class", new Attributes(0))
      man.getEntries().put("p/bar.class", new Attributes(0))
      Using.resource(new JarOutputStream(Files.newOutputStream(f), man)) { jout =>
        val bytes = "hello, world".getBytes
        jout.putNextEntry(new JarEntry("foo.class"))
        jout.write(bytes, 0, bytes.length)
        jout.putNextEntry(new JarEntry("p/bar.class"))
        jout.write(bytes, 0, bytes.length)
        ()
      }
    }
    def manifestAt(location: URI): URL = ScalaClassLoader.fromURLs(List(location.toURL), null).getResource("META-INF/MANIFEST.MF");

    val j = createTestJar();
    Using.resources(ForDeletion(j), new ManifestResources(manifestAt(j.toUri)), new CloseableRegistry) { (_, archive, closeableRegistry) =>
      val settings = new Settings
      val cp = ZipAndJarClassPathFactory.create(archive, settings, closeableRegistry)
      assertTrue(cp.findClass("foo").isDefined)
      assertTrue(cp.findClass("p.bar").isDefined)
    }
  }
}
