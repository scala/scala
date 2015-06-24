import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -cp " + testOutput.path

  override def code = ""

  override def show(): Unit = {
    val g = newCompiler()
    withRun(g)(r => {
      val c = g.rootMirror.getRequiredClass("p.C")
      println(c.info.decls)
      val t = c.info.member(g.newTypeName("T"))
      // this test ensures that the <local child> dummy class symbol is not entered in the
      // scope of trait T during unpickling.
      println(t.info.decls)
    })
  }
}
