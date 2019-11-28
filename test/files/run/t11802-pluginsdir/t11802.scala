
import tools.nsc.plugins.PluginDescription
import tools.partest.DirectTest

import java.nio.file.Files
import java.nio.file.StandardCopyOption.{REPLACE_EXISTING => Replace}
import java.util.jar.{JarEntry, JarOutputStream}
import scala.reflect.io.File

// verify that -Xpluginsdir is respected
//
object Test extends DirectTest {
  override def code = "class Code"

  override def extraSettings = s"-usejavacp -cp ${testOutput.jfile.getAbsolutePath}"

  // plugin named ploogin1_1
  //
  def pluginCode(index: Int) = s"""
    |package t11802 {
    |  class SamplePloogin$index(global: scala.tools.nsc.Global) extends Ploogin(global, s"$${PlooginCounter.named}_$index")
    |  object PlooginCounter {
    |    val count = new java.util.concurrent.atomic.AtomicInteger
    |    def named = s"ploogin$${count.incrementAndGet}"
    |  }
    |}""".stripMargin.trim

  // let partest clean up the temp dir under testOutput
  //
  def compilePlugin(i: Int) = {
    // compile to p1 dir
    val out  = (testOutput / s"p$i").createDirectory()
    val args = List("-usejavacp", "-d", out.path, "-cp", testOutput.path)
    compileString(newCompiler(newSettings(args)))(pluginCode(i))

    val xml  = PluginDescription(s"p$i", s"t11802.SamplePloogin$i").toXML
    (out / "scalac-plugin.xml").toFile writeAll xml

    // copy Ploogin classes, replace under --debug which doesn't remove the output dir
    (testOutput / "t11802").toDirectory.files.foreach(f => Files.copy(f.jfile.toPath, out.jfile.toPath.resolve("t11802").resolve(f.name), Replace))

    // create jar in myplugins
    val plugindir = (testOutput / "myplugins").createDirectory()
    val jar  = plugindir / s"p$i.jar"
    val jout = new JarOutputStream(Files.newOutputStream(jar.jfile.toPath))
    def writeJarEntry(f: File) = {
      val rel = out.jfile.toPath.relativize(f.jfile.toPath)
      jout.putNextEntry(new JarEntry(rel.toString.replace("\\", "/")))  // fix evil windows backslash
      Files.copy(f.jfile.toPath, jout)
    }
    try out.deepFiles.foreach(writeJarEntry) finally jout.close()
    plugindir
  }

  override def show() = {
    val dir = compilePlugin(1)
    compile("-Xpluginsdir", dir.toString, "-Xplugin-require:ploogin1_1")
  }
}

