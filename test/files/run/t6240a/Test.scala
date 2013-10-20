import java.io.File
import scala.sys.process._

object Test extends App {
  def prop(key: String) = {
    val value = System.getProperties.getProperty(key)
    assert(value != null, key)
    value
  }
  val testClassesDir = prop("partest.output")
  assert(new File(testClassesDir).exists, testClassesDir)
  val fullTestClassesClasspath = testClassesDir + prop("path.separator") + prop("java.class.path")
  val javaBinary = if (new File(prop("javacmd")).isAbsolute) prop("javacmd") else prop("java.home") + "/bin/" + prop("javacmd")
  List(javaBinary, "-cp", testClassesDir, "-Dlaunch.classpath=" + fullTestClassesClasspath, "StepOne").!
}