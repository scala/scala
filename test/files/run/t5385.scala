import scala.tools.partest._

object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -Yrangepos"
  override def sources = List(
    "class Azz", "class Bzz ", "class Czz              ", "class Dzz\n",
    "class Ezz{}", "class Fzz{} ", "class Gzz { }", "class Hzz { }            "
  )
  def check(source: String, unit: CompilationUnit) {
    unit.body foreach {
      case cdef: ClassDef => println("%-15s class %s".format(cdef.pos.show, cdef.name))
      case _              =>
    }
  }
}
