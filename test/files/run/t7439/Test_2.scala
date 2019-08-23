import scala.tools.partest._
import java.io.File

object Test extends StoreReporterDirectTest {

  private def classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")

  def code = """
    class C {
      new B_1
    }
  """

  def show(): Unit = {
    // could verify that C compiles without error
    //compileCode(C)
    //assert(filteredInfos.isEmpty, filteredInfos)

    // blow away the entire package
    val a1Class = new File(testOutput.path, "A_1.class")
    assert(a1Class.exists)
    assert(a1Class.delete())
    // testIdent normalizes to separate names using '/' regardless of platform, drops all but last two parts
    log(s"Recompiling after deleting ${a1Class.testIdent}")

    // bad symbolic reference error expected (but no stack trace!)
    // Included a NullPointerException before.
    // Edit: Stub symbol message was reduced, and no longer emitted even under -Xdev
    compile("-cp", classpath, "-Xdev")
    storeReporter.infos.foreach(println)  // empty
  }
}
/* was:
java.lang.NullPointerException
	at scala.reflect.internal.Symbols$Symbol.isMonomorphicType(Symbols.scala:712)
 */
