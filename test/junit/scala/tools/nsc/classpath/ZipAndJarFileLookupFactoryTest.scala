package scala.tools.nsc
package classpath

import org.junit.Test
import java.nio.file._
import java.nio.file.attribute.FileTime
import scala.reflect.io.AbstractFile

class ZipAndJarFileLookupFactoryTest {
  @Test def cacheInvalidation(): Unit = {
    if (scala.util.Properties.isWin) return // can't overwrite an open file on windows.

    val f = Files.createTempFile("test-", ".jar")
    Files.delete(f)
    val g = new scala.tools.nsc.Global(new scala.tools.nsc.Settings())
    assert(!g.settings.YdisableFlatCpCaching.value) // we're testing with our JAR metadata caching enabled.
    def createCp = ZipAndJarClassPathFactory.create(AbstractFile.getFile(f.toFile), g.settings)
    try {
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

      // Change only one letter of contents to force regeneration via a CRC change
      createZip(f, "not empty".getBytes, "p1/C.class")
      val cp3 = createCp
      assert(cp3.findClass("p1.C").isDefined)
      assert(cp3.findClass("p2.X").isDefined)
      assert(cp3.findClass("p3.Y").isDefined)
      assert(cp1 ne cp3, (System.identityHashCode(cp1), System.identityHashCode(cp3)))

      val lastMod1 = Files.getLastModifiedTime(f)
      // Create a new zip at the same path with different contents and last modified
      Files.delete(f)
      createZip(f, Array(), "p1/D.class")
      Files.setLastModifiedTime(f, FileTime.fromMillis(lastMod1.toMillis + 2000))

      // Our classpath cache should create a new instance
      val cp4 = createCp
      assert(cp3 ne cp4, (System.identityHashCode(cp3), System.identityHashCode(cp4)))
      // And that instance should see D, not C, in package p1.
      assert(cp4.findClass("p1.C").isEmpty)
      assert(cp4.findClass("p1.D").isDefined)
    } finally Files.delete(f)
  }

  @Test def createClasspathFor50Jars(): Unit = {
    def time[T](block: => T): T = {
      val start = System.currentTimeMillis
      val res = block
      val totalTime = System.currentTimeMillis - start
      println("Checking for changes in classpath jars took: %1d ms".format(totalTime))
      res
    }

    import scala.collection.JavaConverters._
    val ivyCache = Paths.get(sys.props("user.home")).resolve(".ivy2").resolve("cache")
    val classpath = Files.walk(ivyCache).filter(_.toString.endsWith(".jar")).limit(100).iterator().asScala.toList
    val fileCache = new FileBasedCache[Unit]
    time(fileCache.getOrCreate(classpath, () => ()))
  }

  def createZip(zipLocation: Path, content: Array[Byte], internalPath: String): Unit = {
    val env = new java.util.HashMap[String, String]()
    env.put("create", String.valueOf(Files.notExists(zipLocation)))
    val fileUri = zipLocation.toUri
    val zipUri = new java.net.URI("jar:" + fileUri.getScheme, fileUri.getPath, null)
    val zipfs = FileSystems.newFileSystem(zipUri, env)
    try {
      try {
        val internalTargetPath = zipfs.getPath(internalPath)
        Files.createDirectories(internalTargetPath.getParent)
        Files.write(internalTargetPath, content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE)
      } finally {
        if (zipfs != null) zipfs.close()
      }
    } finally {
      zipfs.close()
    }
  }
}

