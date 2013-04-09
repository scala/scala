import scala.tools.partest.DirectTest
import scala.tools.nsc.util.BatchSourceFile

object Test extends DirectTest {
  // Java code
  override def code = """
    |public @interface MyAnnotation { String value(); }
  """.stripMargin

  override def extraSettings: String = "-usejavacp -Ystop-after:typer -Xprint:parser"

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    System.setErr(prevErr)
  }

  override def newSources(sourceCodes: String*) = {
    assert(sourceCodes.size == 1)
    List(new BatchSourceFile("annodef.java", sourceCodes(0)))
  }
}
