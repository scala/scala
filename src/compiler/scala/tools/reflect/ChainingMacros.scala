package scala.tools.reflect

import scala.reflect.macros.runtime.Context
import scala.reflect.internal.util.Position

abstract class ChainingMacros {
  val c: Context
  val global: c.universe.type = c.universe

  import c.universe.{ Match => _, _ }
  import definitions._
  import treeInfo.Applied

  private def bail(msg: String) = global.abort(msg)

  /**
   * Originally
   *
   *    {{{
   *    { f(self); self }
   *    }}}
   */
  def tap: Tree = transformApplyAppy { (prearg, arg) =>
    val x = TermName(c.freshName("x"))
    arg match {
      case Function(List(vs), body) =>
        val inlineBody = new InlineSymbol(vs.name, vs.symbol, Ident(x)).transform(body)
        val tree = q"""{
          val $x = ($prearg)
          $inlineBody
          $x
        }""".setPos(c.enclosingPosition)
        c.resetLocalAttrs(tree)
      case _ =>
        q"""{
          val $x = $prearg
          $arg($x)
          $x
        }""".setPos(c.enclosingPosition)
    }
  }

  /**
   * Originally
   *
   *    {{{
   *    f(self)
   *    }}}
   */
  def pipe: Tree = transformApplyAppy { (prearg, arg) =>
    val x = TermName(c.freshName("x"))
    arg match {
      case Function(List(vs), body) =>
        val inlineBody = new InlineSymbol(vs.name, vs.symbol, Ident(x)).transform(body)
        val tree = q"""{
          val $x = ($prearg)
          $inlineBody
        }""".setPos(c.enclosingPosition)
        c.resetLocalAttrs(tree)
      case _ =>
        q"""{
          $arg($prearg)
        }""".setPos(c.enclosingPosition)
    }
  }

  def transformApplyAppy(f: (Tree, Tree) => Tree): Tree = {
    c.macroApplication match {
      case Applied(_, targs, List(List(arg))) =>
        c.prefix.tree match {
          case Applied(_, _, List(List(prearg))) =>
            f(prearg, arg)
          case other =>
            bail(s"Unexpected prefix ${showRaw(other)}")
            other
        }
      case other =>
        bail(s"Unexpected application ${showRaw(other)}")
        other
    }
  }

  // InlineSymbol borrowed from https://github.com/non/spire/blob/master/macros/src/main/scala/spire/macros/Syntax.scala
  class InlineSymbol(name: TermName, symbol: Symbol, value: Tree) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case tree: Ident if tree.symbol == symbol =>
        if (tree.name == name) value
        else super.transform(tree)
      case tt: TypeTree if tt.original != null =>
        super.transform(TypeTree().setOriginal(transform(tt.original)))
      case _ =>
        super.transform(tree)
    }
  }
}
