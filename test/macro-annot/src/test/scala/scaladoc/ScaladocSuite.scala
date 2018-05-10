import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._
import java.io._
import java.security.Permission
import scala.tools.nsc.ScalaDoc

@RunWith(classOf[JUnit4])
class ScaladocSuite {
  private def virtualizedPopen(body: => Unit): (Int, String) = {
    val outputStorage = new ByteArrayOutputStream()
    val outputStream = new PrintStream(outputStorage)
    case class SystemExitException(exitCode: Int) extends SecurityException
    val manager = System.getSecurityManager()
    System.setSecurityManager(new SecurityManager {
      override def checkPermission(permission: Permission): Unit = ()
      override def checkPermission(permission: Permission, context: AnyRef): Unit = ()
      override def checkExit(exitCode: Int): Unit = throw new SystemExitException(exitCode)
    })
    try { scala.Console.withOut(outputStream)(scala.Console.withErr(outputStream)(body)); throw new Exception("failed to capture exit code") }
    catch { case SystemExitException(exitCode) => outputStream.close(); (exitCode, outputStorage.toString) }
    finally System.setSecurityManager(manager)
  }

  private def runScaladocTest(testDir: File): Unit = {
    val sources = testDir.listFiles().filter(_.getName.endsWith(".scala")).map(_.getAbsolutePath).toList
    val cp = List("-cp", sys.props("sbt.paths.tests.classpath"))
    val paradise = List("-Ymacro-annotations")
    val tempDir = File.createTempFile("temp", System.nanoTime.toString); tempDir.delete(); tempDir.mkdir()
    val output = List("-d", tempDir.getAbsolutePath)
    val options = cp ++ paradise ++ output ++ sources
    val (exitCode, stdout) = virtualizedPopen(ScalaDoc.main(options.toArray))
    if (exitCode != 0) fail("scaladoc has exited with code " + exitCode + ":\n" + stdout)
  }

  // val resourceDir = new File(System.getProperty("sbt.paths.tests.scaladoc") + File.separatorChar + "resources")
  // val testDirs = resourceDir.listFiles().filter(_.listFiles().nonEmpty).filter(!_.getName().endsWith("_disabled"))
  // testDirs.foreach(testDir => test(testDir.getName)(runScaladocTest(testDir)))
  @Test def simulacrum = runScaladocTest(new File("test/macro-annot/src/test/scala/scaladoc/resources/Simulacrum"))
}
