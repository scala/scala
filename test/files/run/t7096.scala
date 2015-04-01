/*
 * filter: inliner warning; re-run with
 */
import scala.tools.partest._
import scala.tools.nsc._
import scala.reflect.runtime.{universe => ru}
import scala.language.implicitConversions

// necessary to avoid bincompat with scala-partest compiled against the old compiler
abstract class CompilerTest extends DirectTest {
  def check(source: String, unit: global.CompilationUnit): Unit

  lazy val global: Global = newCompiler()
  lazy val units: List[global.CompilationUnit] = compilationUnits(global)(sources: _ *)
  import global._
  import definitions.{ compilerTypeFromTag }

  override def extraSettings = "-usejavacp -d " + testOutput.path

  def show() = (sources, units).zipped foreach check

  // Override at least one of these...
  def code = ""
  def sources: List[String] = List(code)

  // Utility functions
  class MkType(sym: Symbol) {
    def apply[M](implicit t: ru.TypeTag[M]): Type =
      if (sym eq NoSymbol) NoType
      else appliedType(sym, compilerTypeFromTag(t))
  }
  implicit def mkMkType(sym: Symbol) = new MkType(sym)

  def allMembers(root: Symbol): List[Symbol] = {
    def loop(seen: Set[Symbol], roots: List[Symbol]): List[Symbol] = {
      val latest = roots flatMap (_.info.members) filterNot (seen contains _)
      if (latest.isEmpty) seen.toList.sortWith(_ isLess _)
      else loop(seen ++ latest, latest)
    }
    loop(Set(), List(root))
  }

  class SymsInPackage(pkgName: String) {
    def pkg     = rootMirror.getPackage(TermName(pkgName))
    def classes = allMembers(pkg) filter (_.isClass)
    def modules = allMembers(pkg) filter (_.isModule)
    def symbols = classes ++ terms filterNot (_ eq NoSymbol)
    def terms   = allMembers(pkg) filter (s => s.isTerm && !s.isConstructor)
    def tparams = classes flatMap (_.info.typeParams)
    def tpes    = symbols map (_.tpe) distinct
  }
}

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
    exitingTyper {
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
