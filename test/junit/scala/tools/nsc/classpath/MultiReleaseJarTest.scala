package scala.tools.nsc.classpath

import java.io.ByteArrayOutputStream
import java.nio.file.{FileSystems, Files, Path}
import java.util.jar.Attributes
import java.util.jar.Attributes.Name

import org.junit.{Assert, Test}

import scala.tools.nsc.{Global, Settings}
import scala.tools.testing.BytecodeTesting
import scala.util.Properties

class MultiReleaseJarTest extends BytecodeTesting {
  import compiler._
  @Test
  def mrJar(): Unit = {
    if (!Properties.isJavaAtLeast("9")) { println("skipping mrJar() on old JdK"); return} // TODO test that the compiler warns that --release is unsupported.

    val temp1 = Files.createTempFile("mr-jar-test-", ".jar")

    // TODO test fails if both Global runs look at the same JAR on disk. Caching problem in our classpath implementation?
    // val temp2 = temp1
    val temp2 = Files.createTempFile("mr-jar-test-", ".jar")

    try {
      def code(newApi: String) = s"package p1; abstract class Versioned { def oldApi: Int; $newApi }"

      val oldC = compileToBytes(code("")).head._2
      val newC = compileToBytes(code("def newApi: Int")).head._2
      List(temp1, temp2).foreach(temp => createZip(temp, List(
        "/p1/Versioned.class" -> oldC,
        "/META-INF/versions/9/p1/Versioned.class" -> newC,
        "/META-INF/MANIFEST.MF" -> createManifest)
      ))

      def declsOfC(jarPath: Path, release: String) = {
        val settings = new Settings()
        settings.usejavacp.value = true
        settings.classpath.value = jarPath.toAbsolutePath.toString
        val g = new Global(settings)
        settings.release.value = release
        new g.Run
        val decls = g.rootMirror.staticClass("p1.Versioned").info.decls.filterNot(_.isConstructor).map(_.name.toString).toList.sorted
        decls
      }

      Assert.assertEquals(List("newApi", "oldApi"), declsOfC(temp1, "9"))
      Assert.assertEquals(List("oldApi"), declsOfC(temp2, "8"))
    } finally
      List(temp1, temp2).foreach(Files.deleteIfExists)
  }

  @Test
  def ctSymTest(): Unit = {
    if (!Properties.isJavaAtLeast("9")) { println("skipping mrJar() on old JDK"); return} // TODO test that the compiler warns that --release is unsupported.

    def lookup(className: String, release: String): Boolean = {
      val settings = new Settings()
      settings.usejavacp.value = true
      val g = new Global(settings)
      import g._
      settings.release.value = release
      new Run
      rootMirror.getClassIfDefined(TypeName(className)) != NoSymbol
    }
    Assert.assertTrue(lookup("java.lang.invoke.LambdaMetafactory", "8"))
    Assert.assertFalse(lookup("java.lang.invoke.LambdaMetafactory", "7"))
    Assert.assertTrue(lookup("java.lang.invoke.LambdaMetafactory", "9"))
  }

  private def createManifest = {
    val manifest = new java.util.jar.Manifest()
    manifest.getMainAttributes.put(Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(new Attributes.Name("Multi-Release"), String.valueOf(true))
    val os = new ByteArrayOutputStream()
    manifest.write(os)
    val manifestBytes = os.toByteArray
    manifestBytes
  }
  private def createZip(zipLocation: Path, content: List[(String, Array[Byte])]): Unit = {
    val env = new java.util.HashMap[String, String]()
    Files.deleteIfExists(zipLocation)
    env.put("create", String.valueOf(true))
    val fileUri = zipLocation.toUri
    val zipUri = new java.net.URI("jar:" + fileUri.getScheme, fileUri.getPath, null)
    val zipfs = FileSystems.newFileSystem(zipUri, env)
    try {
      try {
        for ((internalPath, contentBytes) <- content) {
          val internalTargetPath = zipfs.getPath(internalPath)
          Files.createDirectories(internalTargetPath.getParent)
          Files.write(internalTargetPath, contentBytes)
        }
      } finally {
        if (zipfs != null) zipfs.close()
      }
    } finally {
      zipfs.close()
    }
  }



}
