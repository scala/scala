import scala.tools.partest._

object Test extends DirectTest {
  override def code = ""
  override def extraSettings: String = "-usejavacp"

  override def show() {
    val c = newCompiler()
    new c.Run
    import c._

    val f4 = typeOf[Three].member(newTermName("f4"))
    val f4ParamInfo = f4.paramss.head.head.info
    println(f4ParamInfo.baseClasses)
    println(f4ParamInfo.baseTypeSeq)
  }
}

