import scala.reflect.internal.util.BatchSourceFile
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  // Java code
  override def code = """
    |public @interface MyAnnotation { String value(); }
  """.stripMargin

  override def extraSettings: String = "-usejavacp -Ystop-after:namer -Vprint:parser"

  override def newSources(sourceCodes: String*) = {
    assert(sourceCodes.size == 1)
    List(new BatchSourceFile("annodef.java", sourceCodes(0)))
  }

  override def show(): Unit = if (!compile()) println("Compilation failed!")
}
