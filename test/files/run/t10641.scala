
import scala.tools.partest.DirectTest

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import javax.tools._

import scala.jdk.CollectionConverters._
import scala.util.chaining._

object Test extends DirectTest {
  def jcode =
    """
public class J_0 {
    public int _() { return 42; }
    public String funkyJavaNameFactory() { return "funk"; }
    public int underscore() { return _(); }
}
    """.trim()
  def code =
    """
class S_1 {
  val j = new J_0
  import j.{`_` => u, funkyJavaNameFactory => f}   // was: Wildcard import must be in last position
  val x = u
  def y = f
}
    """.trim()

  override def extraSettings = s"-usejavacp -classpath ${testOutput.path}"

  def show() =
    // (use of '_' as an identifier might not be supported in releases after Java SE 8)
    testUnderJavaAtLeast("9") {
      ()
    } otherwise {
      //val jfile = testOutput.jfile.toPath.resolve("J_0.java").tap(Files.writeString(_, jcode, UTF_8)
      val javac = ToolProvider.getSystemJavaCompiler()
      val fm    = javac.getStandardFileManager(null, null, UTF_8)  // null diagnostics, locale
      val opts  = List("-d", testOutput.path).asJava
      val uri   = URI.create("string:///J_0.java")
      val kind  = JavaFileObject.Kind.SOURCE
      val unit  = new SimpleJavaFileObject(uri, kind) { override def getCharContent(ignore: Boolean) = jcode }
      val units = List(unit).asJava
      val task  = javac.getTask(null, fm, null, opts, null, units) // null out, diagnostics, classes

      assert(task.call())
      assert(compile())
    }
}
