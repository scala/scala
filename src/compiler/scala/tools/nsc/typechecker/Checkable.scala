/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import Checkability._
import scala.language.postfixOps

/** On pattern matcher checkability:
 *
 *  The spec says that case _: List[Int] should be always issue
 *  an unchecked warning:
 *
 *  > Types which are not of one of the forms described above are
 *  > also accepted as type patterns. However, such type patterns
 *  > will be translated to their erasure (§3.7). The Scala compiler
 *  > will issue an “unchecked” warning for these patterns to flag
 *  > the possible loss of type-safety.
 *
 *  But the implementation goes a little further to omit warnings
 *  based on the static type of the scrutinee. As a trivial example:
 *
 *    def foo(s: Seq[Int]) = s match { case _: List[Int] => }
 *
 *  need not issue this warning.
 *
 *  Consider a pattern match of this form: (x: X) match { case _: P => }
 *
 *  There are four possibilities to consider:
 *     [P1] X will always conform to P
 *     [P2] x will never conform to P
 *     [P3] X will conform to P if some runtime test is true
 *     [P4] X cannot be checked against P
 *
 *  The first two cases correspond to those when there is enough
 *  static information to say X <: P or that (x ∈ X) ⇒ (x ∉ P).
 *  The fourth case includes unknown abstract types or structural
 *  refinements appearing within a pattern.
 *
 *  The third case is the interesting one.  We designate another type, XR,
 *  which is essentially the intersection of X and |P|, where |P| is
 *  the erasure of P.  If XR <: P, then no warning is emitted.
 *
 *  We evaluate "X with conform to P" by checking `X <: P_wild`, where
 *  P_wild is the result of substituting wildcard types in place of
 *  pattern type variables. This is intentionally stricter than
 *  (X matchesPattern P), see SI-8597 for motivating test cases.
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
  import CheckabilityChecker.{ isNeverSubType, isNeverSubClass }

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
      if (tps1.size != tps2.size)
        devWarning(s"Unequally sized type arg lists in propagateKnownTypes($from, $to): ($tps1, $tps2)")

      (tps1, tps2).zipped foreach (_ =:= _)
      // Alternate, variance respecting formulation causes
      // neg/unchecked3.scala to fail (abstract types).  TODO -
      // figure it out. It seems there is more work to do if I
      // allow for variance, because the constraints accumulate
      // as bounds and "tvar.instValid" is false.
      //
      // foreach3(tps1, tps2, bc.typeParams)((tp1, tp2, tparam) =>
      //   if (tparam.initialize.isCovariant) tp1 <:< tp2
      //   else if (tparam.isContravariant) tp2 <:< tp1
      //   else tp1 =:= tp2
      // )
    }

    val resArgs = tparams zip tvars map {
      case (_, tvar) if tvar.instValid => tvar.constr.inst
      case (tparam, _)                 => tparam.tpeHK
    }
    appliedType(to, resArgs: _*)
  }

  private def isUnwarnableTypeArgSymbol(sym: Symbol) = (
       sym.isTypeParameter                     // dummy
    || (sym.name.toTermName == nme.WILDCARD)   // _
    || nme.isVariableName(sym.name)            // type variable
  )
  private def isUnwarnableTypeArg(arg: Type) = (
       uncheckedOk(arg)                                 // @unchecked T
    || isUnwarnableTypeArgSymbol(arg.typeSymbolDirect)  // has to be direct: see pos/t1439
  )
  private def uncheckedOk(tp: Type) = tp hasAnnotation UncheckedClass

  private def typeArgsInTopLevelType(tp: Type): List[Type] = {
    val tps = tp match {
      case RefinedType(parents, _)              => parents flatMap typeArgsInTopLevelType
      case TypeRef(_, ArrayClass, arg :: Nil)   => if (arg.typeSymbol.isAbstractType) arg :: Nil else typeArgsInTopLevelType(arg)
      case TypeRef(pre, sym, args)              => typeArgsInTopLevelType(pre) ++ args
      case ExistentialType(tparams, underlying) => tparams.map(_.tpe) ++ typeArgsInTopLevelType(underlying)
      case _                                    => Nil
    }
    tps filterNot isUnwarnableTypeArg
  }

  private def scrutConformsToPatternType(scrut: Type, pattTp: Type): Boolean = {
    def typeVarToWildcard(tp: Type) = {
      // The need for typeSymbolDirect is demonstrated in neg/t8597b.scala
      if (tp.typeSymbolDirect.isPatternTypeVariable) WildcardType else tp
    }
    val pattTpWild = pattTp.map(typeVarToWildcard)
    scrut <:< pattTpWild
  }

  private class CheckabilityChecker(val X: Type, val P: Type) {
    def Xsym = X.typeSymbol
    def Psym = P.typeSymbol
    def PErased = {
      P match {
        case erasure.GenericArray(n, core) => existentialAbstraction(core.typeSymbol :: Nil, P)
        case _ => existentialAbstraction(Psym.typeParams, Psym.tpe_*)
      }
    }
    def XR   = if (Xsym == AnyClass) PErased else propagateKnownTypes(X, Psym)


    // sadly the spec says (new java.lang.Boolean(true)).isInstanceOf[scala.Boolean]
    def P1   = scrutConformsToPatternType(X, P)
    def P2   = !Psym.isPrimitiveValueClass && isNeverSubType(X, P)
    def P3   = isNonRefinementClassType(P) && scrutConformsToPatternType(XR, P)
    def P4   = !(P1 || P2 || P3)

    def summaryString = f"""
      |Checking checkability of (x: $X) against pattern $P
      |[P1] $P1%-6s X <: P             // $X  <: $P
      |[P2] $P2%-6s x ∉ P              // (x ∈ $X) ⇒ (x ∉ $P)
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
        debuglog("Checkability checker says 'Uncheckable', but uncheckable type cannot be found:\n" + summaryString)
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

    def neverSubClass = isNeverSubClass(Xsym, Psym)
    def neverMatches  = result == StaticallyFalse
    def isUncheckable = result == Uncheckable
    def isCheckable   = !isUncheckable
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
    /** Are these symbols classes with no subclass relationship? */
    def areUnrelatedClasses(sym1: Symbol, sym2: Symbol) = (
         sym1.isClass
      && sym2.isClass
      && !(sym1 isSubClass sym2)
      && !(sym2 isSubClass sym1)
    )
    /** Are all children of these symbols pairwise irreconcilable? */
    def allChildrenAreIrreconcilable(sym1: Symbol, sym2: Symbol) = (
      sym1.sealedChildren.toList forall (c1 =>
        sym2.sealedChildren.toList forall (c2 =>
          areIrreconcilableAsParents(c1, c2)
        )
      )
    )
    /** Is it impossible for the given symbols to be parents in the same class?
     *  This means given A and B, can there be an instance of A with B? This is the
     *  case if neither A nor B is a subclass of the other, and one of the following
     *  additional conditions holds:
     *   - either A or B is effectively final
     *   - neither A nor B is a trait (i.e. both are actual classes, not eligible for mixin)
     *   - both A and B are sealed/final, and every possible pairing of their children is irreconcilable
     *
     *  TODO: the last two conditions of the last possibility (that the symbols are not of
     *  classes being compiled in the current run) are because this currently runs too early,
     *  and .children returns Nil for sealed classes because their children will not be
     *  populated until typer.  It was too difficult to move things around for the moment,
     *  so I will consult with moors about the optimal time to be doing this.
     */
    def areIrreconcilableAsParents(sym1: Symbol, sym2: Symbol): Boolean = areUnrelatedClasses(sym1, sym2) && (
         isEffectivelyFinal(sym1) // initialization important
      || isEffectivelyFinal(sym2)
      || !sym1.isTrait && !sym2.isTrait
      || isSealedOrFinal(sym1) && isSealedOrFinal(sym2) && allChildrenAreIrreconcilable(sym1, sym2) && !currentRun.compiles(sym1) && !currentRun.compiles(sym2)
    )
    private def isSealedOrFinal(sym: Symbol) = sym.isSealed || sym.isFinal
    private def isEffectivelyFinal(sym: Symbol): Boolean = (
      // initialization important
      sym.initialize.isEffectivelyFinalOrNotOverridden || (
        settings.future && isTupleSymbol(sym) // SI-7294 step into the future and treat TupleN as final.
      )
    )

    def isNeverSubClass(sym1: Symbol, sym2: Symbol) = areIrreconcilableAsParents(sym1, sym2)

    private def isNeverSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[Symbol]): Boolean = /*logResult(s"isNeverSubArgs($tps1, $tps2, $tparams)")*/ {
      def isNeverSubArg(t1: Type, t2: Type, variance: Variance) = (
        if (variance.isInvariant) isNeverSameType(t1, t2)
        else if (variance.isCovariant) isNeverSubType(t2, t1)
        else if (variance.isContravariant) isNeverSubType(t1, t2)
        else false
      )
      exists3(tps1, tps2, tparams map (_.variance))(isNeverSubArg)
    }
    private def isNeverSameType(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
      case (TypeRef(_, sym1, args1), TypeRef(_, sym2, args2)) =>
        isNeverSubClass(sym1, sym2) || ((sym1 == sym2) && isNeverSubArgs(args1, args2, sym1.typeParams))
      case _ =>
        false
    }
    // Important to dealias at any entry point (this is the only one at this writing.)
    def isNeverSubType(tp1: Type, tp2: Type): Boolean = /*logResult(s"isNeverSubType($tp1, $tp2)")*/((tp1.dealias, tp2.dealias) match {
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

    def isUncheckable(P0: Type) = !isCheckable(P0)

    def isCheckable(P0: Type): Boolean = (
      uncheckedOk(P0) || (P0.widen match {
        case TypeRef(_, NothingClass | NullClass | AnyValClass, _) => false
        case RefinedType(_, decls) if !decls.isEmpty               => false
        case RefinedType(parents, _)                               => parents forall isCheckable
        case p                                                     => new CheckabilityChecker(AnyTpe, p) isCheckable
      })
    )

    /** TODO: much better error positions.
     *  Kind of stuck right now because they just pass us the one tree.
     *  TODO: Eliminate inPattern, canRemedy, which have no place here.
     */
    def checkCheckable(tree: Tree, P0: Type, X0: Type, inPattern: Boolean, canRemedy: Boolean = false) {
      if (uncheckedOk(P0)) return
      def where = if (inPattern) "pattern " else ""

      // singleton types not considered here, dealias the pattern for SI-XXXX
      val P = P0.dealiasWiden
      val X = X0.widen

      def PString = if (P eq P0) P.toString else s"$P (the underlying of $P0)"

      P match {
        // Prohibit top-level type tests for these, but they are ok nested (e.g. case Foldable[Nothing] => ... )
        case TypeRef(_, NothingClass | NullClass | AnyValClass, _) =>
          InferErrorGen.TypePatternOrIsInstanceTestError(tree, P)
        // If top-level abstract types can be checked using a classtag extractor, don't warn about them
        case TypeRef(_, sym, _) if sym.isAbstractType && canRemedy =>
          ;
        // Matching on types like case _: AnyRef { def bippy: Int } => doesn't work -- yet.
        case RefinedType(_, decls) if !decls.isEmpty =>
          reporter.warning(tree.pos, s"a pattern match on a refinement type is unchecked")
        case RefinedType(parents, _) =>
          parents foreach (p => checkCheckable(tree, p, X, inPattern, canRemedy))
        case _ =>
          val checker = new CheckabilityChecker(X, P)
          if (checker.result == RuntimeCheckable)
            log(checker.summaryString)

          if (checker.neverMatches) {
            val addendum = if (checker.neverSubClass) "" else " (but still might match its erasure)"
            reporter.warning(tree.pos, s"fruitless type test: a value of type $X cannot also be a $PString$addendum")
          }
          else if (checker.isUncheckable) {
            val msg = (
              if (checker.uncheckableType =:= P) s"abstract type $where$PString"
              else s"${checker.uncheckableMessage} in type $where$PString"
            )
            reporter.warning(tree.pos, s"$msg is unchecked since it is eliminated by erasure")
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
