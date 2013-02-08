import scala.tools.partest._
import scala.tools.nsc._

object Test extends CompilerTest {
  import global._
  import definitions._

  override def code = """
package ano

class ann(x: Any) extends annotation.TypeConstraint

abstract class Base {
  def foo(x: String): String @ann(x.trim())
}

class Sub extends Base {
  def foo(x: String): String @ann(x.trim()) = x
}
  """

  object syms extends SymsInPackage("ano")
  import syms._

  def check(source: String, unit: global.CompilationUnit) {
    afterTyper {
      terms.filter(_.name.toString == "foo").foreach(sym => {
        val xParam = sym.tpe.paramss.flatten.head
        val annot = sym.tpe.finalResultType.annotations.head
        val xRefs = annot.args.head.filter(t => t.symbol == xParam)
        println(s"testing symbol ${sym.ownerChain}, param $xParam, xRefs $xRefs")
        assert(xRefs.length == 1, xRefs)
      })
    }
  }
}
