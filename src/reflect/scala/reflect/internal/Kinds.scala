/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import scala.reflect.internal.util.StringOps.{ countAsString, countElementsAsString }

trait Kinds {
  self: SymbolTable =>

  import definitions._

  private type SymPair = ((Symbol, Symbol)) // ((Argument, Parameter))

  case class KindErrors(
         arity: List[SymPair] = Nil,
      variance: List[SymPair] = Nil,
    strictness: List[SymPair] = Nil
  ) {
    def isEmpty = arity.isEmpty && variance.isEmpty && strictness.isEmpty

    def arityError(syms: SymPair)      = copy(arity = arity :+ syms)
    def varianceError(syms: SymPair)   = copy(variance = variance :+ syms)
    def strictnessError(syms: SymPair) = copy(strictness = strictness :+ syms)

    def ++(errs: KindErrors) = KindErrors(
      arity ++ errs.arity,
      variance ++ errs.variance,
      strictness ++ errs.strictness
    )
    // @M TODO this method is duplicated all over the place (varianceString)
    private def varStr(s: Symbol): String =
      if (s.isCovariant) "covariant"
      else if (s.isContravariant) "contravariant"
      else "invariant";

    private def qualify(a0: Symbol, b0: Symbol): String = if (a0.toString != b0.toString) "" else {
      if((a0 eq b0) || (a0.owner eq b0.owner)) ""
      else {
        var a = a0; var b = b0
        while (a.owner.name == b.owner.name) { a = a.owner; b = b.owner}
        if (a.locationString ne "") " (" + a.locationString.trim + ")" else ""
      }
    }
    private def kindMessage(a: Symbol, p: Symbol)(f: (String, String) => String): String =
      f(a+qualify(a,p), p+qualify(p,a))

    // Normally it's nicer to print nothing rather than '>: Nothing <: Any' all over
    // the place, but here we need it for the message to make sense.
    private def strictnessMessage(a: Symbol, p: Symbol) =
      kindMessage(a, p)("%s's bounds%s are stricter than %s's declared bounds%s".format(
        _, a.info, _, p.info match {
          case tb @ TypeBounds(_, _) if tb.isEmptyBounds  => " >: Nothing <: Any"
          case tb                                         => "" + tb
        })
      )

    private def varianceMessage(a: Symbol, p: Symbol) =
      kindMessage(a, p)("%s is %s, but %s is declared %s".format(_, varStr(a), _, varStr(p)))

    private def arityMessage(a: Symbol, p: Symbol) =
      kindMessage(a, p)("%s has %s, but %s has %s".format(
        _, countElementsAsString(a.typeParams.length, "type parameter"),
        _, countAsString(p.typeParams.length))
      )

    private def buildMessage(xs: List[SymPair], f: (Symbol, Symbol) => String) = (
      if (xs.isEmpty) ""
      else xs map f.tupled mkString ("\n", ", ", "")
    )

    def errorMessage(targ: Type, tparam: Symbol): String = (
        (targ+"'s type parameters do not match "+tparam+"'s expected parameters:")
      + buildMessage(arity, arityMessage)
      + buildMessage(variance, varianceMessage)
      + buildMessage(strictness, strictnessMessage)
    )
  }
  val NoKindErrors = KindErrors(Nil, Nil, Nil)

  // TODO: this desperately needs to be cleaned up
  // plan: split into kind inference and subkinding
  // every Type has a (cached) Kind
  def kindsConform(tparams: List[Symbol], targs: List[Type], pre: Type, owner: Symbol): Boolean =
    checkKindBounds0(tparams, targs, pre, owner, false).isEmpty

  /** Check whether `sym1`'s variance conforms to `sym2`'s variance.
   *
   *  If `sym2` is invariant, `sym1`'s variance is irrelevant. Otherwise they must be equal.
   */
  private def variancesMatch(sym1: Symbol, sym2: Symbol) = (
       sym2.variance==0
    || sym1.variance==sym2.variance
  )

  /** Check well-kindedness of type application (assumes arities are already checked) -- @M
   *
   * This check is also performed when abstract type members become concrete (aka a "type alias") -- then tparams.length==1
   * (checked one type member at a time -- in that case, prefix is the name of the type alias)
   *
   * Type application is just like value application: it's "contravariant" in the sense that
   * the type parameters of the supplied type arguments must conform to the type parameters of
   * the required type parameters:
   *   - their bounds must be less strict
   *   - variances must match (here, variances are absolute, the variance of a type parameter does not influence the variance of its higher-order parameters)
   *   - @M TODO: are these conditions correct,sufficient&necessary?
   *
   *  e.g. class Iterable[t, m[+x <: t]] --> the application Iterable[Int, List] is okay, since
   *       List's type parameter is also covariant and its bounds are weaker than <: Int
   */
  def checkKindBounds0(
    tparams: List[Symbol],
    targs: List[Type],
    pre: Type,
    owner: Symbol,
    explainErrors: Boolean
  ): List[(Type, Symbol, KindErrors)] = {

    // instantiate type params that come from outside the abstract type we're currently checking
    def transform(tp: Type, clazz: Symbol): Type = tp.asSeenFrom(pre, clazz)

    // check that the type parameters hkargs to a higher-kinded type conform to the
    // expected params hkparams
    def checkKindBoundsHK(
      hkargs:        List[Symbol],
      arg:           Symbol,
      param:         Symbol,
      paramowner:    Symbol,
      underHKParams: List[Symbol],
      withHKArgs:    List[Symbol]
    ): KindErrors = {

      var kindErrors: KindErrors = NoKindErrors
      def bindHKParams(tp: Type) = tp.substSym(underHKParams, withHKArgs)
      // @M sometimes hkargs != arg.typeParams, the symbol and the type may
      // have very different type parameters
      val hkparams = param.typeParams

      def kindCheck(cond: Boolean, f: KindErrors => KindErrors) {
        if (!cond)
          kindErrors = f(kindErrors)
      }

      if (settings.debug.value) {
        log("checkKindBoundsHK expected: "+ param +" with params "+ hkparams +" by definition in "+ paramowner)
        log("checkKindBoundsHK supplied: "+ arg +" with params "+ hkargs +" from "+ owner)
        log("checkKindBoundsHK under params: "+ underHKParams +" with args "+ withHKArgs)
      }

      if (!sameLength(hkargs, hkparams)) {
        // Any and Nothing are kind-overloaded
        if (arg == AnyClass || arg == NothingClass) NoKindErrors
        // shortcut: always set error, whether explainTypesOrNot
        else return kindErrors.arityError(arg -> param)
      }
      else foreach2(hkargs, hkparams) { (hkarg, hkparam) =>
        if (hkparam.typeParams.isEmpty && hkarg.typeParams.isEmpty) { // base-case: kind *
          kindCheck(variancesMatch(hkarg, hkparam), _ varianceError (hkarg -> hkparam))
          // instantiateTypeParams(tparams, targs)
          //   higher-order bounds, may contain references to type arguments
          // substSym(hkparams, hkargs)
          //   these types are going to be compared as types of kind *
          //
          // Their arguments use different symbols, but are
          // conceptually the same. Could also replace the types by
          // polytypes, but can't just strip the symbols, as ordering
          // is lost then.
          val declaredBounds     = transform(hkparam.info.instantiateTypeParams(tparams, targs).bounds, paramowner)
          val declaredBoundsInst = transform(bindHKParams(declaredBounds), owner)
          val argumentBounds     = transform(hkarg.info.bounds, owner)

          kindCheck(declaredBoundsInst <:< argumentBounds, _ strictnessError (hkarg -> hkparam))

          debuglog(
            "checkKindBoundsHK base case: " + hkparam +
            " declared bounds: " + declaredBounds +
            " after instantiating earlier hkparams: " + declaredBoundsInst + "\n" +
            "checkKindBoundsHK base case: "+ hkarg +
            " has bounds: " + argumentBounds
          )
        }
        else {
          hkarg.initialize // SI-7902 otherwise hkarg.typeParams yields List(NoSymbol)!
          debuglog("checkKindBoundsHK recursing to compare params of "+ hkparam +" with "+ hkarg)
          kindErrors ++= checkKindBoundsHK(
            hkarg.typeParams,
            hkarg,
            hkparam,
            paramowner,
            underHKParams ++ hkparam.typeParams,
            withHKArgs ++ hkarg.typeParams
          )
        }
        if (!explainErrors && !kindErrors.isEmpty)
          return kindErrors
      }
      if (explainErrors) kindErrors
      else NoKindErrors
    }

    if (settings.debug.value && (tparams.nonEmpty || targs.nonEmpty)) log(
      "checkKindBounds0(" + tparams + ", " + targs + ", " + pre + ", "
      + owner + ", " + explainErrors + ")"
    )

    flatMap2(tparams, targs) { (tparam, targ) =>
      // Prevent WildcardType from causing kind errors, as typevars may be higher-order
      if (targ == WildcardType) Nil else {
        // force symbol load for #4205
        targ.typeSymbolDirect.info
        // @M must use the typeParams of the *type* targ, not of the *symbol* of targ!!
        val tparamsHO = targ.typeParams
        if (targ.isHigherKinded || tparam.typeParams.nonEmpty) {
          // NOTE: *not* targ.typeSymbol, which normalizes
          val kindErrors = checkKindBoundsHK(
            tparamsHO, targ.typeSymbolDirect, tparam,
            tparam.owner, tparam.typeParams, tparamsHO
          )
          if (kindErrors.isEmpty) Nil else {
            if (explainErrors) List((targ, tparam, kindErrors))
            // Return as soon as an error is seen if there's nothing to explain.
            else return List((NoType, NoSymbol, NoKindErrors))
          }
        }
        else Nil
      }
    }
  }
}
