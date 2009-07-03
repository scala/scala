/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import scala.tools.nsc.util.Position

/** Helper methods that build trees for pattern matching.
 *
 *  @author Burak Emir
 */
trait CodeFactory extends ast.TreeDSL
{
  self: transform.ExplicitOuter with PatternNodes =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import definitions._
  import CODE._

  // strip bindings to find what tree lies underneath
  def unbind(x: Tree): Tree = x match {
    case Bind(_, y) => unbind(y)
    case y          => y
  }

  final def typedValDef(x: Symbol, rhs: Tree)(implicit typer: Typer) = {
    val finalRhs = x.tpe match {
      case WildcardType   =>
        rhs setType null
        x setInfo (typer typed rhs).tpe
        rhs
      case _              =>
        typer.typed(rhs, x.tpe)
    }
    typer typed (VAL(x) === finalRhs)
  }

  final def squeezedBlock(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree =
    if (settings_squeeze) Block(Nil, squeezedBlock1(vds, exp))
    else                  Block(vds, exp)

  final def squeezedBlock1(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree = {
    class RefTraverser(sym: Symbol) extends Traverser {
      var nref, nsafeRef = 0
      override def traverse(tree: Tree) = tree match {
        case t: Ident if t.symbol eq sym =>
          nref += 1
          if (sym.owner == currentOwner) // oldOwner should match currentOwner
            nsafeRef += 1

        case LabelDef(_, args, rhs) =>
          (args dropWhile(_.symbol ne sym)) match {
            case Nil  =>
            case _    => nref += 2  // cannot substitute this one
          }
          traverse(rhs)
        case t if nref > 1 =>       // abort, no story to tell
        case t =>
          super.traverse(t)
      }
    }

    class Subst(sym: Symbol, rhs: Tree) extends Transformer {
      var stop = false
      override def transform(tree: Tree) = tree match {
        case t: Ident if t.symbol == sym =>
          stop = true
          rhs
        case _ => if (stop) tree else super.transform(tree)
      }
    }

    lazy val squeezedTail = squeezedBlock(vds.tail, exp)
    def default = squeezedTail match {
      case Block(vds2, exp2) => Block(vds.head :: vds2, exp2)
      case exp2              => Block(vds.head :: Nil,  exp2)
    }

    if (vds.isEmpty) exp
    else vds.head match {
      case vd: ValDef =>
        val sym = vd.symbol
        val rt = new RefTraverser(sym)
        rt.atOwner (theOwner) (rt traverse squeezedTail)

        rt.nref match {
          case 0                      => squeezedTail
          case 1 if rt.nsafeRef == 1  => new Subst(sym, vd.rhs) transform squeezedTail
          case _                      => default
        }
      case _          =>
        default
    }
  }
}
