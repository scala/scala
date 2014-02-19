import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._, Flag._, internal.reificationSupport.setSymbol

class QuasiquoteProperties(name: String) extends Properties(name) with ArbitraryTreesAndNames with Helpers

trait Helpers {
  /** Runs a code block and returns proof confirmation
   *  if no exception has been thrown while executing code
   *  block. This is useful for simple one-off tests.
   */
  def test[T](block: => T) =
    Prop { params =>
      block
      Result(Prop.Proof)
    }

  object simplify extends Transformer {
    object SimplifiedName {
      val st = scala.reflect.runtime.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
      val FreshName = new st.FreshNameExtractor
      def unapply[T <: Name](name: T): Option[T] = name.asInstanceOf[st.Name] match {
        case FreshName(prefix) =>
          Some((if (name.isTermName) TermName(prefix) else TypeName(prefix)).asInstanceOf[T])
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      case Ident(SimplifiedName(name))                  => Ident(name)
      case ValDef(mods, SimplifiedName(name), tpt, rhs) => ValDef(mods, name, transform(tpt), transform(rhs))
      case Bind(SimplifiedName(name), rhs)              => Bind(name, rhs)
      case _ =>
        super.transform(tree)
    }

    def apply(tree: Tree): Tree = transform(tree)
  }

  implicit class TestSimilarTree(tree1: Tree) {
    def ≈(tree2: Tree) = simplify(tree1).equalsStructure(simplify(tree2))
  }

  implicit class TestSimilarListTree(lst: List[Tree]) {
    def ≈(other: List[Tree]) = (lst.length == other.length) && lst.zip(other).forall { case (t1, t2) => t1 ≈ t2 }
  }

  implicit class TestSimilarListListTree(lst: List[List[Tree]]) {
    def ≈(other: List[List[Tree]]) = (lst.length == other.length) && lst.zip(other).forall { case (l1, l2) => l1 ≈ l2 }
  }

  implicit class TestSimilarName(name: Name) {
    def ≈(other: Name) = name == other
  }

  implicit class TestSimilarMods(mods: Modifiers) {
    def ≈(other: Modifiers) = (mods.flags == other.flags) && (mods.privateWithin ≈ other.privateWithin) && (mods.annotations ≈ other.annotations)
  }

  def assertThrows[T <: AnyRef](f: => Any)(implicit manifest: Manifest[T]): Unit = {
    val clazz = manifest.runtimeClass.asInstanceOf[Class[T]]
    val thrown =
      try {
        f
        false
      } catch {
        case u: Throwable =>
          if (!clazz.isAssignableFrom(u.getClass))
            assert(false, s"wrong exception: $u")
          true
      }
    if(!thrown)
      assert(false, "exception wasn't thrown")
  }

  def assertEqAst(tree: Tree, code: String) = assert(eqAst(tree, code))
  def eqAst(tree: Tree, code: String) = tree ≈ parse(code)

  val toolbox = currentMirror.mkToolBox()
  val parse = toolbox.parse(_)
  val compile = toolbox.compile(_)
  val eval = toolbox.eval(_)

  def typecheck(tree: Tree) = toolbox.typecheck(tree)

  def typecheckTyp(tree: Tree) = {
    val q"type $_ = $res" = typecheck(q"type T = $tree")
    res
  }

  def typecheckPat(tree: Tree) = {
    val q"$_ match { case $res => }" = typecheck(q"((): Any) match { case $tree => }")
    res
  }

  def fails(msg: String, block: String) = {
    def result(ok: Boolean, description: String = "") = {
      val status = if (ok) Prop.Proof else Prop.False
      val labels = if (description != "") Set(description) else Set.empty[String]
      Prop { new Prop.Result(status, Nil, Set.empty, labels) }
    }
    try {
      compile(parse(s"""
        object Wrapper extends Helpers {
          import scala.reflect.runtime.universe._
          $block
        }
      """))
      result(false, "given code doesn't fail to typecheck")
    } catch {
      case ToolBoxError(emsg, _) =>
        if (!emsg.contains(msg))
          result(false, s"error message '${emsg}' is not the same as expected '$msg'")
        else
          result(true)
    }
  }

  val scalapkg = setSymbol(Ident(TermName("scala")), definitions.ScalaPackage)
}
