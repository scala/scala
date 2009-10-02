/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter

trait PatternOptimizer extends ast.TreeDSL
{
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import symtab.Flags
  import CODE._

  private[matching] trait Squeezer {
    self: MatrixContext =>

    def squeezedBlock(vds: List[Tree], exp: Tree): Tree =
      if (settings_squeeze) Block(Nil, squeezedBlock1(vds, exp))
      else                  Block(vds, exp)

    private def squeezedBlock1(vds: List[Tree], exp: Tree): Tree = {
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
          rt.atOwner (owner) (rt traverse squeezedTail)

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

  private[matching] trait MatchMatrixOptimizer {
    self: MatchMatrix =>

    import self.context._

    final def optimize(tree: Tree): Tree = {
      // Extractors which can spot pure true/false expressions
      // even through the haze of braces
      abstract class SeeThroughBlocks[T] {
        protected def unapplyImpl(x: Tree): T
        def unapply(x: Tree): T = x match {
          case Block(Nil, expr)         => unapply(expr)
          case _                        => unapplyImpl(x)
        }
      }
      object IsTrue extends SeeThroughBlocks[Boolean] {
        protected def unapplyImpl(x: Tree): Boolean = x equalsStructure TRUE
      }
      object IsFalse extends SeeThroughBlocks[Boolean] {
        protected def unapplyImpl(x: Tree): Boolean = x equalsStructure FALSE
      }
      object lxtt extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case blck @ Block(vdefs, ld @ LabelDef(name, params, body)) =>
            def shouldInline(t: FinalState) = t.isReachedOnce && (t.label eq ld.symbol)

            if (targets exists shouldInline) squeezedBlock(vdefs, body)
            else blck

          case t =>
            super.transform(t match {
              // note - it is too early for any other true/false related optimizations
              case If(cond, IsTrue(), IsFalse())  => cond

              case If(cond1, If(cond2, thenp, elsep1), elsep2) if (elsep1 equalsStructure elsep2) =>
                IF (cond1 AND cond2) THEN thenp ELSE elsep1
              case If(cond1, If(cond2, thenp, Apply(jmp, Nil)), ld: LabelDef) if jmp.symbol eq ld.symbol =>
                IF (cond1 AND cond2) THEN thenp ELSE ld
              case t => t
          })
        }
      }
      object resetTraverser extends Traverser {
        import Flags._
        def reset(vd: ValDef) =
          if (vd.symbol hasFlag SYNTHETIC) vd.symbol resetFlag (TRANS_FLAG|MUTABLE)

        override def traverse(x: Tree): Unit = x match {
          case vd: ValDef => reset(vd)
          case _          => super.traverse(x)
        }
      }

      returning[Tree](resetTraverser traverse _)(lxtt transform tree)
    }
  }
}