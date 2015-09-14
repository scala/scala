import scala.tools.partest.ParserTest
import scala.reflect.internal.util.BatchSourceFile

object Test extends ParserTest {
  // Java code
  override def code = """
    |public @interface MyAnnotation { String value(); }
  """.stripMargin

  override def extraSettings: String = "-usejavacp -Ystop-after:namer -Xprint:parser"

  override def newSources(sourceCodes: String*) = {
    assert(sourceCodes.size == 1)
    List(new BatchSourceFile("annodef.java", sourceCodes(0)))
  }
}
