package scala.tools.nsc

import java.io.ByteArrayOutputStream
import java.nio.file._
import java.util.jar.Attributes
import java.util.jar.Attributes.Name

import org.junit.Assert.fail
import org.junit.Test

import scala.tools.nsc
import scala.tools.testing.{AssertUtil, BytecodeTesting, ClearAfterClass, CompilerErrors}
import scala.util.Properties

class JpmsTest extends ClearAfterClass {

  private val fileFactory: FileFactory = cached("sourceFileFactory", () => new FileFactory())

  // TODO JPMS factor out the setup part of this test into a test fixture and split this into smaller pieces
  @Test def modulePath(): Unit = {
    if (!Properties.isJavaAtLeast("9")) { println("skipping modulePath() on old JDK"); return }

    val javaClassPath = sys.props("java.class.path").split(java.io.File.pathSeparator).toList
    val library = javaClassPath.find(element => Paths.get(element).getFileName.toString == "library").get
    val outputDir = fileFactory.tempDir()
    val scalaLibraryJpmsModuleJar = createDummyAutomaticModule(outputDir, "scala.library")
    def createCompiler(extraArgs: String, outputDir: Path, modulePath: List[String] = Nil) = {
      val fullModulePath = (scalaLibraryJpmsModuleJar :: modulePath).mkString(java.io.File.pathSeparator)
      val compiler = BytecodeTesting.newCompiler(extraArgs = s"-modulepath $fullModulePath -patchmodule:scala.library=$library $extraArgs")
      compiler.outputFunction = () => nsc.io.AbstractFile.getDirectory(outputDir.toFile)
      compiler
    }
    val code1 = fileFactory(
      """
        |package p1 {
        |  package impl {
        |    class C {
        |      def foo = javax.xml.bind.DatatypeConverter.printBase64Binary _
        |    }
        |  }
        |  package api {
        |    class P1Api
        |  }
        |}
      """.stripMargin
    )

    val compilerDefault = createCompiler("", outputDir)

    {
      val thrown = AssertUtil.assertThrows[CompilerErrors](compilerDefault.compileSourceFiles(code1 :: Nil))
      thrown.messages.toList match {
        case msg :: Nil => assert(msg.contains("class DatatypeConverter in package bind cannot be accessed in package javax.xml.bind"), msg)
        case msgs => fail(msgs.mkString("\n"))
      }
    }

    val compilerAddModules = createCompiler("-addmodules:java.xml.bind", outputDir)
    compilerAddModules.compileSourceFiles(code1 :: Nil)

    val moduleInfoP1 = fileFactory("module-info.java",
      """
        |module p1 {
        |  requires java.xml.bind;
        |  requires scala.library;
        |  exports p1.api;
        |}
      """.stripMargin)

    // Jointly compile module-info.java and a.scala with Scalac
    compilerDefault.compileSourceFiles(code1 :: moduleInfoP1 :: Nil)

    // Use javac to generate the module-info.class file.
    // /* TODO: JDK11+
//    compilerDefault.compileJava(moduleInfoP1.file.file.toPath :: Nil, List("-nowarn", "-d", outputDir.toString,
//      "--module-path", scalaLibraryJpmsModuleJar.toString, "--patch-module", s"scala.library=$library", "-cp", outputDir.toString))
//    assert(Files.exists(outputDir.resolve("module-info.class")))

    // Let's compile clients of this module.
    val outputDir2 = fileFactory.tempDir()

    // If our client is in the unnamed module, it reads all modules automatically, so can access the exported p1.api
    {
      val compiler = createCompiler("-addmodules:p1", outputDir2, List(outputDir.toString))
      compiler.compileSourceFiles(fileFactory("class Client { new p1.api.P1Api }") :: Nil)
    }

    // Let's add a reference to a package isn't exported.
    val clientCode = fileFactory("""package other; class Patched { new p1.api.P1Api; new p1.impl.C}""")

    // The client source, again part of the unnamed module, can access `p1.api` but can't access `p1.impl`.
    {
      val compiler = createCompiler("-addmodules:p1", outputDir2, List(outputDir.toString))
      val thrown = AssertUtil.assertThrows[CompilerErrors](compiler.compileSourceFiles(clientCode :: Nil))
      thrown.messages.toList match {
        case msg :: Nil => assert(msg.contains("class C in package impl cannot be accessed in package p1.impl"), msg)
        case msgs => fail(msgs.mkString("\n"))
      }
    }

    // We can add an the export with a command line option:
    {
      val compiler = createCompiler("-addmodules:p1 -addexports:p1/p1.impl=ALL-UNNAMED", outputDir2, List(outputDir.toString))
      compiler.compileSourceFiles(clientCode :: Nil)
    }

    // Or, more idiomatically, patch the unit test sources into module `p1`. Then, they can access everything in the module.
    {
      val compiler = createCompiler(s"-addmodules:p1 -patchmodule:p1=${fileFactory.baseDirectory}", outputDir2, List(outputDir.toString))
      // TODO JPMS check whether javac requires an explicit `-addmodules` here, or whether modules referred to in
      // `-patchmodules` are implicitly added to the root modules of the graph.
      compiler.compileSourceFiles(clientCode :: Nil)
    }
  }

  private def createDummyAutomaticModule(tempDir: Path, value: String) = {
    val zip = tempDir.resolve(value.replaceAllLiterally(".", "-") + ".jar")
    createZip(zip, List("/META-INF/MANIFEST.MF" -> createManifest(value)))
    zip
  }
  private def createManifest(automaticModuleName: String): Array[Byte] = {
    val manifest = new java.util.jar.Manifest()
    manifest.getMainAttributes.put(Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(new Attributes.Name("Automatic-Module-Name"), automaticModuleName)
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
