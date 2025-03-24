/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.patmat

import scala.tools.nsc.symtab.Flags.SYNTHETIC

/** Factory methods used by TreeMakers to make the actual trees. */
trait MatchCodeGen extends Interface {
  import global._

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // generate actual trees
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait CodegenCore extends MatchMonadInterface {
    private var ctr = 0
    def freshName(prefix: String) = { ctr += 1; newTermName(s"$prefix$ctr") }

    // assert(owner ne null); assert(owner ne NoSymbol)
    def freshSym(pos: Position, tp: Type = NoType, prefix: String = "x") =
      NoSymbol.newTermSymbol(freshName(prefix), pos, newFlags = SYNTHETIC) setInfo tp

    def newSynthCaseLabel(name: String) =
      NoSymbol.newLabel(freshName(name), NoPosition) setFlag treeInfo.SYNTH_CASE_FLAGS

    // codegen relevant to the structure of the translation (how extractors are combined)
    trait AbsCodegen {
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree

      // local / context-free
      def _asInstanceOf(b: Symbol, tp: Type): Tree
      def _equals(checker: Tree, binder: Symbol): Tree
      def _isInstanceOf(b: Symbol, tp: Type): Tree
      def drop(tgt: Tree)(n: Int): Tree
      def index(tgt: Tree)(i: Int): Tree
      def mkZero(tp: Type): Tree
      def tupleSel(binder: Symbol)(i: Int): Tree
    }

    // structure
    trait Casegen extends AbsCodegen { import CODE._
      def one(res: Tree): Tree

      def flatMap(prev: Tree, b: Symbol, next: Tree): Tree
      def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree
      def flatMapGuard(cond: Tree, next: Tree): Tree
      def flatMapCondStored(cond: Tree, condSym: Symbol, res: Tree, nextBinder: Symbol, next: Tree): Tree
      def ifThenElseZero(c: Tree, thenp: Tree): Tree = {
        val z = zero
        thenp match {
          case If(c1, thenp1, elsep1) if z equalsStructure elsep1 =>
            If(c AND c1, thenp1, elsep1) // cleaner, leaner trees
          case _ =>
            If(c, thenp, zero)
        }
      }
      protected def zero: Tree
    }

    def codegen: AbsCodegen

    abstract class CommonCodegen extends AbsCodegen { import CODE._
      def fun(arg: Symbol, body: Tree): Tree     = Function(List(ValDef(arg)), body)
      def tupleSel(binder: Symbol)(i: Int): Tree = (REF(binder) DOT nme.productAccessorName(i)) // make tree that accesses the i'th component of the tuple referenced by binder
      def index(tgt: Tree)(i: Int): Tree         = tgt APPLY (LIT(i))
      def drop(tgt: Tree)(n: Int): Tree          = fn(tgt, nme.drop, LIT(n))

      // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
      def _equals(checker: Tree, binder: Symbol): Tree = checker MEMBER_== REF(binder)

      // the force is needed mainly to deal with the GADT typing hack (we can't detect it otherwise as tp nor pt need contain an abstract type, we're just casting wildly)
      def _asInstanceOf(b: Symbol, tp: Type): Tree = if (b.info <:< tp) REF(b) else gen.mkCastPreservingAnnotations(REF(b), tp)
      def _isInstanceOf(b: Symbol, tp: Type): Tree = gen.mkIsInstanceOf(REF(b), tp.withoutAnnotations, any = true, wrapInApply = false)

      def mkZero(tp: Type): Tree = gen.mkConstantZero(tp) match {
        case Constant(null) => gen.mkAsInstanceOf(Literal(Constant(null)), tp, any = true, wrapInApply = false) // the magic incantation is true/false here
        case const          => Literal(const)
      }
    }
  }


  trait OptimizedCodegen extends CodegenCore with TypedSubstitution with MatchMonadInterface {
    override def codegen: AbsCodegen = optimizedCodegen

    // when we know we're targeting Option, do some inlining the optimizer won't do
    // for example, `o.flatMap(f)` becomes `if(o == None) None else f(o.get)`, similarly for orElse and guard
    //   this is a special instance of the advanced inlining optimization that takes a method call on
    //   an object of a type that only has two concrete subclasses, and inlines both bodies, guarded by an if to distinguish the two cases
    object optimizedCodegen extends CommonCodegen { import CODE._

      /** Inline runOrElse and get rid of Option allocations
       *
       * runOrElse(scrut: scrutTp)(matcher): resTp = matcher(scrut) getOrElse \${catchAll(`scrut`)}
       * the matcher's optional result is encoded as a flag, keepGoing, where keepGoing == true encodes result.isEmpty,
       * if keepGoing is false, the result Some(x) of the naive translation is encoded as matchRes == x
       */
      def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree = {
        val matchRes = NoSymbol.newValueParameter(newTermName("x"), NoPosition, newFlags = SYNTHETIC) setInfo restpe.withoutAnnotations
        val matchEnd = newSynthCaseLabel("matchEnd") setInfo MethodType(List(matchRes), restpe)

        def newCaseSym = newSynthCaseLabel("case") setInfo MethodType(Nil, restpe)
        var _currCase = newCaseSym

        val caseDefs = cases map { (mkCase: Casegen => Tree) =>
          val currCase = _currCase
          val nextCase = newCaseSym
          _currCase = nextCase

          LabelDef(currCase, Nil, mkCase(new OptimizedCasegen(matchEnd, nextCase)))
        }
        // must compute catchAll after caseLabels (side-effects nextCase)
        // catchAll.isEmpty iff no synthetic default case needed (the (last) user-defined case is a default)
        // if the last user-defined case is a default, it will never jump to the next case; it will go immediately to matchEnd
        val catchAllDef = matchFailGen.map { matchFailGen =>
          val scrutRef = scrutSym.fold(EmptyTree: Tree)(REF) // for alternatives

          LabelDef(_currCase, Nil, matchEnd APPLY matchFailGen(scrutRef))
        }.toList // at most 1 element

        // scrutSym == NoSymbol when generating an alternatives matcher
        val scrutDef = scrutSym.fold(List[Tree]())(ValDef(_, scrut) :: Nil) // for alternatives

        // the generated block is taken apart in TailCalls under the following assumptions
        // the assumption is once we encounter a case, the remainder of the block will consist of cases
        // the prologue may be empty, usually it is the valdef that stores the scrut
        // val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        Block(
          scrutDef ++ caseDefs ++ catchAllDef,
          LabelDef(matchEnd, List(matchRes), REF(matchRes))
        )
      }

      class OptimizedCasegen(matchEnd: Symbol, nextCase: Symbol) extends CommonCodegen with Casegen {
        def matcher(scrut: Tree, scrutSym: Symbol, restpe: Type)(cases: List[Casegen => Tree], matchFailGen: Option[Tree => Tree]): Tree =
          optimizedCodegen.matcher(scrut, scrutSym, restpe)(cases, matchFailGen)

        // only used to wrap the RHS of a body
        // res: T
        // returns MatchMonad[T]
        def one(res: Tree): Tree = matchEnd.APPLY(res) // a jump to a case label is special-cased in typedApply
        protected def zero: Tree = nextCase.APPLY()

        // prev: MatchMonad[T]
        // b: T
        // next: MatchMonad[U]
        // returns MatchMonad[U]
        def flatMap(prev: Tree, b: Symbol, next: Tree): Tree = {
          val prevSym = freshSym(prev.pos, prev.tpe, "o")
          BLOCK(
            ValDef(prevSym, prev),
            // must be isEmpty and get as we don't control the target of the call (prev is an extractor call)
            ifThenElseZero(
              NOT(prevSym DOT nme.isEmpty),
              Substitution(b, prevSym DOT nme.get)(next)
            )
          )
        }

        // cond: Boolean
        // res: T
        // nextBinder: T
        // next == MatchMonad[U]
        // returns MatchMonad[U]
        def flatMapCond(cond: Tree, res: Tree, nextBinder: Symbol, next: Tree): Tree = {
          val rest = (
            // only emit a local val for `nextBinder` if it's actually referenced in `next`
            if (next.exists(_.symbol eq nextBinder))
              Block(ValDef(nextBinder, res) :: Nil, next)
            else next
          )
          ifThenElseZero(cond, rest)
        }

        // guardTree: Boolean
        // next: MatchMonad[T]
        // returns MatchMonad[T]
        def flatMapGuard(guardTree: Tree, next: Tree): Tree =
          ifThenElseZero(guardTree, next)

        def flatMapCondStored(cond: Tree, condSym: Symbol, res: Tree, nextBinder: Symbol, next: Tree): Tree =
          ifThenElseZero(cond, BLOCK(
            condSym    === TRUE,
            nextBinder === res,
            next
          ))
      }

    }
  }
}
