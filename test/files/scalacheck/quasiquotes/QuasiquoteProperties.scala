import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.runtime.universe.Flag._
import scala.reflect.runtime.currentMirror
import scala.reflect.api.{Liftable, Universe}
import scala.reflect.macros.TypecheckException
import scala.tools.reflect.{ToolBox, ToolBoxError}

import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

class QuasiquoteProperties(name: String) extends Properties(name) with ArbitraryTreesAndNames with Helpers

trait Helpers {
  /** Runs a code block and returns proof confirmation
   *  if no exception has been thrown while executing code
   *  block. This is useful for simple one-off tests.
   */
  def test[T](block: => T)=
    Prop { (params) =>
      block
      Result(Prop.Proof)
    }

  implicit class TestSimilarTree(tree1: Tree) {
    def ≈(tree2: Tree) = tree1.equalsStructure(tree2)
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
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
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

  def annot(name: String): Tree = annot(TypeName(name), Nil)
  def annot(name: TypeName): Tree = annot(name, Nil)
  def annot(name: String, args: List[Tree]): Tree = annot(TypeName(name), args)
  def annot(name: TypeName, args: List[Tree]): Tree = q"new $name(..$args)"
}