/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import symtab.Flags._
import scala.annotation.tailrec
import Checkability._

/** On pattern matcher checkability:
 *
 *  Consider a pattern match of this form: (x: X) match { case _: P => }
 *
 *  There are four possibilities to consider:
 *     [P1] X will always conform to P
 *     [P2] x will never conform to P
 *     [P3] X <: P if some runtime test is true
 *     [P4] X cannot be checked against P
 *
 *  The first two cases correspond to those when there is enough static
 *  information to say X <: P or that !(X <: P) for all X and P.
 *  The fourth case includes unknown abstract types or structural
 *  refinements appearing within a pattern.
 *
 *  The third case is the interesting one.  We designate another type, XR,
 *  which is essentially the intersection of X and |P|, where |P| is
 *  the erasure of P.  If XR <: P, then no warning is emitted.
 *
 *  Examples of how this info is put to use:
 *  sealed trait A[T] ; class B[T] extends A[T]
 *    def f(x: B[Int]) = x match { case _: A[Int] if true => }
 *    def g(x: A[Int]) = x match { case _: B[Int] => }
 *
 *  `f` requires no warning because X=B[Int], P=A[Int], and B[Int] <:< A[Int].
 *  `g` requires no warning because X=A[Int], P=B[Int], XR=B[Int], and B[Int] <:< B[Int].
 *      XR=B[Int] because a value of type A[Int] which is tested to be a B can
 *      only be a B[Int], due to the definition of B (B[T] extends A[T].)
 *
 *  This is something like asSeenFrom, only rather than asking what a type looks
 *  like from the point of view of one of its base classes, we ask what it looks
 *  like from the point of view of one of its subclasses.
 */
trait Checkable {
  self: Analyzer =>

  import global._
  import definitions._

  /** The applied type of class 'to' after inferring anything
   *  possible from the knowledge that 'to' must also be of the
   *  type given in 'from'.
   */
  def propagateKnownTypes(from: Type, to: Symbol): Type = {
    def tparams  = to.typeParams
    val tvars    = tparams map (p => TypeVar(p))
    val tvarType = appliedType(to, tvars: _*)
    val bases    = from.baseClasses filter (to.baseClasses contains _)

    bases foreach { bc =>
      val tps1 = (from baseType bc).typeArgs
      val tps2 = (tvarType baseType bc).typeArgs
      (tps1, tps2).zipped foreach (_ =:= _)
    }

    val resArgs = tparams zip tvars map {
      case (_, tvar) if tvar.instValid => tvar.constr.inst
      case (tparam, _)                 => tparam.tpe
    }
    appliedType(to, resArgs: _*)
  }

  private def isUnwarnableTypeArgSymbol(sym: Symbol) = (
       sym.isTypeParameterOrSkolem             // dummy
    || (sym.name.toTermName == nme.WILDCARD)   // _
    || nme.isVariableName(sym.name)            // type variable
  )
  private def isUnwarnableTypeArg(arg: Type) = (
       isUnwarnableTypeArgSymbol(arg.typeSymbolDirect)   // has to be direct: see pos/t1439
    || (arg hasAnnotation UncheckedClass)                // @unchecked T
  )

  private def typeArgsInTopLevelType(tp: Type): List[Type] = {
    val tps = tp match {
      case RefinedType(parents, _)              => parents flatMap typeArgsInTopLevelType
      case TypeRef(_, ArrayClass, arg :: Nil)   => typeArgsInTopLevelType(arg)
      case TypeRef(pre, sym, args)              => typeArgsInTopLevelType(pre) ++ args
      case ExistentialType(tparams, underlying) => tparams.map(_.tpe) ++ typeArgsInTopLevelType(underlying)
      case _                                    => Nil
    }
    tps filterNot isUnwarnableTypeArg
  }

  private class CheckabilityChecker(val X: Type, val P: Type) {
    def Xsym = X.typeSymbol
    def Psym = P.typeSymbol
    def XR   = propagateKnownTypes(X, Psym)
    def P1   = X matchesPattern P
    def P2   = CheckabilityChecker.isNeverSubType(X, P)
    def P3   = Psym.isClass && (XR matchesPattern P)
    def P4   = !(P1 || P2 || P3)

    def summaryString = f"""
      |Checking checkability of (x: $X) against pattern $P
      |[P1] $P1%-6s X <: P             // $X  <: $P
      |[P2] $P2%-6s X !<: P            // $X !<: $P for all X, P
      |[P3] $P3%-6s XR <: P            // $XR <: $P
      |[P4] $P4%-6s None of the above  // !(P1 || P2 || P3)
    """.stripMargin.trim

    val result = (
      if (X.isErroneous || P.isErroneous) CheckabilityError
      else if (P1) StaticallyTrue
      else if (P2) StaticallyFalse
      else if (P3) RuntimeCheckable
      else if (uncheckableType == NoType) {
        // Avoid warning (except ourselves) if we can't pinpoint the uncheckable type
        debugwarn("Checkability checker says 'Uncheckable', but uncheckable type cannot be found:\n" + summaryString)
        CheckabilityError
      }
      else Uncheckable
    )
    lazy val uncheckableType = if (Psym.isAbstractType) P else {
      val possibles = typeArgsInTopLevelType(P).toSet
      val opt = possibles find { targ =>
        // Create a derived type with every possibly uncheckable type replaced
        // with a WildcardType, except for 'targ'. If !(XR <: derived) then
        // 'targ' is uncheckable.
        val derived = P map (tp => if (possibles(tp) && !(tp =:= targ)) WildcardType else tp)
        !(XR <:< derived)
      }
      opt getOrElse NoType
    }

    def neverMatches = result == StaticallyFalse
    def isUncheckable = result == Uncheckable
    def uncheckableMessage = uncheckableType match {
      case NoType                                   => "something"
      case tp @ RefinedType(_, _)                   => "refinement " + tp
      case TypeRef(_, sym, _) if sym.isAbstractType => "abstract type " + sym.name
      case tp                                       => "non-variable type argument " + tp
    }
  }

  /** X, P, [P1], etc. are all explained at the top of the file.
   */
  private object CheckabilityChecker {
    /** Given classes A and B, can it be shown A is never a subclass of B?
     */
    private def isNeverSubClass(sym1: Symbol, sym2: Symbol) = /*logResult(s"isNeverSubClass($sym1, $sym2)")*/(
         sym1.isClass
      && sym2.isClass
      && !(sym1 isSubClass sym2)
      && (
        // If B is final, A can only be a subclass of B if it is B itself.
        // Therefore, A <: B is impossible unless B <: A.
        if (sym2.isEffectivelyFinal)
          !(sym2 isSubClass sym1)
        // If A is final but B is not, a subclass relationship can
        // still be ruled out if B is sealed and A is not a subclass of
        // any of B's sealed children.
        else (
             sym1.isEffectivelyFinal
          && sym2.isSealed
          && sym2.children.forall(child => !(sym1 isSubClass child))
        )
      )
    )
    private def isNeverSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[Symbol]): Boolean = /*logResult(s"isNeverSubArgs($tps1, $tps2, $tparams)")*/{
      def isNeverSubArg(t1: Type, t2: Type, variance: Int) = {
        if (variance > 0) isNeverSubType(t2, t1)
        else if (variance < 0) isNeverSubType(t1, t2)
        else isNeverSameType(t1, t2)
      }
      exists3(tps1, tps2, tparams map (_.variance))(isNeverSubArg)
    }
    private def isNeverSameType(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
      case (TypeRef(_, sym1, args1), TypeRef(_, sym2, args2)) =>
        (    isNeverSubClass(sym1, sym2)
          || isNeverSubClass(sym2, sym1)
          || ((sym1 == sym2) && isNeverSubArgs(args1, args2, sym1.typeParams))
        )
      case _ =>
        false
    }
    // Important to dealias at any entry point (this is the only one at this writing.)
    private def isNeverSubType(tp1: Type, tp2: Type): Boolean = /*logResult(s"isNeverSubType($tp1, $tp2)")*/((tp1.dealias, tp2.dealias) match {
      case (TypeRef(_, sym1, args1), TypeRef(_, sym2, args2)) =>
        isNeverSubClass(sym1, sym2) || {
          (sym1 isSubClass sym2) && {
            val tp1seen = tp1 baseType sym2
            isNeverSubArgs(tp1seen.typeArgs, args2, sym2.typeParams)
          }
        }
      case _ => false
    })
  }

  trait InferCheckable {
    self: Inferencer =>

    /** TODO: much better error positions.
     *  Kind of stuck right now because they just pass us the one tree.
     *  TODO: Eliminate inPattern, canRemedy, which have no place here.
     */
    def checkCheckable(tree: Tree, P0: Type, X: Type, inPattern: Boolean, canRemedy: Boolean = false) {
      def where = if (inPattern) "pattern " else ""

      // singleton types not considered here
      val P = P0.widen
      P match {
        // Prohibit top-level type tests for these, but they are ok nested (e.g. case Foldable[Nothing] => ... )
        case TypeRef(_, NothingClass | NullClass | AnyValClass, _) =>
          InferErrorGen.TypePatternOrIsInstanceTestError(tree, P)
        // If top-level abstract types can be checked using a classtag extractor, don't warn about them
        case TypeRef(_, sym, _) if sym.isAbstractType && canRemedy =>
          ;
        case _ =>
          val checker = new CheckabilityChecker(X.widen, P)
          log(checker.summaryString)
          if (checker.neverMatches)
            getContext.unit.warning(tree.pos, s"fruitless type test: a $X can never be a $P (but still might match its erasure)")
          else if (checker.isUncheckable) {
            val msg = (
              if (checker.uncheckableType =:= P) s"abstract type $where$P"
              else s"${checker.uncheckableMessage} in type $where$P"
            )
            getContext.unit.warning(tree.pos, s"$msg is unchecked since it is eliminated by erasure")
          }
      }
    }
  }
}

private[typechecker] final class Checkability(val value: Int) extends AnyVal { }
private[typechecker] object Checkability {
  val StaticallyTrue    = new Checkability(0)
  val StaticallyFalse   = new Checkability(1)
  val RuntimeCheckable  = new Checkability(2)
  val Uncheckable       = new Checkability(3)
  val CheckabilityError = new Checkability(4)
}
