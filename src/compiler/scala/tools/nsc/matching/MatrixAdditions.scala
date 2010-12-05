/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import PartialFunction._

/** Traits which are mixed into MatchMatrix, but separated out as
 *  (somewhat) independent components to keep them on the sidelines.
 */
trait MatrixAdditions extends ast.TreeDSL
{
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import symtab.Flags
  import CODE._
  import Debug._
  import treeInfo.{ IsTrue, IsFalse }
  import definitions.{ isValueClass }

  /** The Squeezer, responsible for all the squeezing.
   */
  private[matching] trait Squeezer {
    self: MatrixContext =>

    private val settings_squeeze = !settings.Ynosqueeze.value

    def squeezedBlockPVs(pvs: List[PatternVar], exp: Tree): Tree =
      squeezedBlock(pvs map (_.valDef), exp)

    /** Compresses multiple Blocks. */
    def mkBlock(stats: List[Tree], expr: Tree): Tree = expr match {
      case Block(stats1, expr1) if stats.isEmpty  => mkBlock(stats1, expr1)
      case _                                      => Block(stats, expr)
    }

    def squeezedBlock(vds: List[Tree], exp: Tree): Tree =
      if (settings_squeeze) mkBlock(Nil, squeezedBlock1(vds, exp))
      else                  mkBlock(vds, exp)

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

  /** The Optimizer, responsible for some of the optimizing.
   */
  private[matching] trait MatchMatrixOptimizer {
    self: MatchMatrix =>

    import self.context._

    final def optimize(tree: Tree): Tree = {
      object lxtt extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case blck @ Block(vdefs, ld @ LabelDef(name, params, body)) =>
            def shouldInline(t: FinalState) = t.isReachedOnce && (t.labelSym eq ld.symbol)

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
      returning(lxtt transform tree)(_ => clearSyntheticSyms())
    }
  }

  /** The Exhauster.
   */
  private[matching] trait MatrixExhaustiveness {
    self: MatchMatrix =>

    import self.context._

    /** Exhaustiveness checking requires looking for sealed classes
     *  and if found, making sure all children are covered by a pattern.
     */
    class ExhaustivenessChecker(rep: Rep) {
      val Rep(tvars, rows) = rep

      import Flags.{ MUTABLE, ABSTRACT, SEALED }

      private case class Combo(index: Int, sym: Symbol) {
        val isBaseClass = sym.tpe.baseClasses.toSet

        // is this combination covered by the given pattern?
        def isCovered(p: Pattern) = {
          def coversSym = isBaseClass(decodedEqualsType(p.tpe).typeSymbol)

          cond(p.tree) {
            case _: UnApply | _: ArrayValue => true
            case x                          => p.isDefault || coversSym
          }
        }
      }

      /* True if the patterns in 'row' cover the given type symbol combination, and has no guard. */
      private def rowCoversCombo(row: Row, combos: List[Combo]) =
        row.guard.isEmpty && (combos forall (c => c isCovered row.pats(c.index)))

      private def requiresExhaustive(sym: Symbol) = {
         (sym.isMutable) &&                 // indicates that have not yet checked exhaustivity
        !(sym hasFlag NO_EXHAUSTIVE) &&        // indicates @unchecked
         (sym.tpe.typeSymbol.isSealed) &&
        !isValueClass(sym.tpe.typeSymbol)   // make sure it's not a primitive, else (5: Byte) match { case 5 => ... } sees no Byte
      }

      private lazy val inexhaustives: List[List[Combo]] = {
        // let's please not get too clever side-effecting the mutable flag.
        val toCollect = tvars.zipWithIndex filter { case (pv, i) => requiresExhaustive(pv.sym) }
        val collected = toCollect map { case (pv, i) =>
          // okay, now reset the flag
          pv.sym resetFlag MUTABLE
          // have to filter out children which cannot match: see ticket #3683 for an example
          val kids = pv.tpe.typeSymbol.sealedDescendants filter (_.tpe matchesPattern pv.tpe)

          i -> kids
        }

        val folded =
          collected.foldRight(List[List[Combo]]())((c, xs) => {
            val (i, syms) = c match { case (i, set) => (i, set.toList) }
            xs match {
              case Nil  => syms map (s => List(Combo(i, s)))
              case _    => for (s <- syms ; rest <- xs) yield Combo(i, s) :: rest
            }
          })

        folded filterNot (combo => rows exists (r => rowCoversCombo(r, combo)))
      }

      private def mkPad(xs: List[Combo], i: Int): String = xs match {
        case Nil                    => pad("*")
        case Combo(j, sym) :: rest  => if (j == i) pad(sym.name.toString) else mkPad(rest, i)
      }
      private def mkMissingStr(open: List[Combo]) =
        "missing combination %s\n" format tvars.indices.map(mkPad(open, _)).mkString

      /** The only public method. */
      def check = {
        def errMsg = (inexhaustives map mkMissingStr).mkString
        if (inexhaustives.nonEmpty)
          cunit.warning(tvars.head.lhs.pos, "match is not exhaustive!\n" + errMsg)

        rep
      }
    }
  }
}