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

package scala
package reflect
package internal

import scala.annotation.nowarn
import scala.reflect.internal.util.StringOps.{countAsString, countElementsAsString}

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
      else "invariant"

    private def qualify(a0: Symbol, b0: Symbol): String = if (a0.toString != b0.toString) "" else {
      if((a0 eq b0) || (a0.owner eq b0.owner)) ""
      else {
        var a = a0; var b = b0
        while (a.owner.name == b.owner.name) { a = a.owner; b = b.owner}
        if (a.locationString ne "") " (" + a.locationString.trim + ")" else ""
      }
    }
    private def kindMessage(a: Symbol, p: Symbol)(f: (String, String) => String): String =
      f(a.toString+qualify(a,p), p.toString+qualify(p,a))

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
      else xs.map(f.tupled).mkString("\n", ", ", "")
    )

    def errorMessage(targ: Type, tparam: Symbol): String = (
        (s"${targ}'s type parameters do not match ${tparam}'s expected parameters:")
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
    checkKindBounds0(tparams, targs, pre, owner, explainErrors = false).isEmpty

  /** Check whether `sym1`'s variance conforms to `sym2`'s variance.
   *
   *  If `sym2` is invariant, `sym1`'s variance is irrelevant. Otherwise they must be equal.
   */
  private def variancesMatch(sym1: Symbol, sym2: Symbol) = (
       sym2.variance.isInvariant
    || sym1.variance == sym2.variance
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
  @nowarn("cat=lint-nonlocal-return")
  def checkKindBounds0(
    tparams: List[Symbol],
    targs: List[Type],
    pre: Type,
    owner: Symbol,
    explainErrors: Boolean
  ): List[(Type, Symbol, KindErrors)] = {

    // check that the type parameters hkargs to a higher-kinded type conform to the
    // expected params hkparams
    @nowarn("cat=lint-nonlocal-return")
    def checkKindBoundsHK(
      hkargs:        List[Symbol],
      arg:           Symbol,
      argPre:        Type,
      argOwner:      Symbol,
      param:         Symbol,
      paramOwner:    Symbol,
      underHKParams: List[Symbol],
      withHKArgs:    List[Symbol],
      flip:          Boolean
    ): KindErrors = {

      var kindErrors: KindErrors = NoKindErrors
      // @M sometimes hkargs != arg.typeParams, the symbol and the type may
      // have very different type parameters
      val hkparams = param.typeParams

      def kindCheck(cond: Boolean, f: KindErrors => KindErrors): Unit =
        if (!cond) kindErrors = f(kindErrors)

      if (settings.isDebug) {
        log("checkKindBoundsHK expected: "+ param +" with params "+ hkparams +" by definition in "+ paramOwner)
        log("checkKindBoundsHK supplied: "+ arg +" with params "+ hkargs +" from "+ argOwner)
        log("checkKindBoundsHK under params: "+ underHKParams +" with args "+ withHKArgs)
      }

      if (!sameLength(hkargs, hkparams)) return {
        // Any and Nothing are kind-overloaded
        if (arg == AnyClass || arg == NothingClass) NoKindErrors
        // shortcut: always set error, whether explainTypesOrNot
        else kindErrors.arityError(arg -> param)
      }
      else foreach2(hkargs, hkparams) { (hkarg, hkparam) =>
        if (hkparam.typeParams.isEmpty && hkarg.typeParams.isEmpty) { // base-case: kind *
          if (flip) kindCheck(variancesMatch(hkparam, hkarg), _.varianceError(hkparam -> hkarg))
          else kindCheck(variancesMatch(hkarg, hkparam), _.varianceError(hkarg -> hkparam))

          // instantiateTypeParams(tparams, targs)
          //   higher-order bounds, may contain references to type arguments
          // substSym(hkparams, hkargs)
          //   these types are going to be compared as types of kind *
          //
          // Their arguments use different symbols, but are
          // conceptually the same. Could also replace the types by
          // polytypes, but can't just strip the symbols, as ordering
          // is lost then.
          val declaredBounds     = hkparam.info.instantiateTypeParams(tparams, targs).bounds.asSeenFrom(pre, paramOwner)
          val declaredBoundsInst = declaredBounds.substSym(underHKParams, withHKArgs).asSeenFrom(pre, owner)
          val argumentBounds     = hkarg.info.bounds.asSeenFrom(argPre, argOwner).asSeenFrom(pre, owner)

          if (flip) kindCheck(argumentBounds <:< declaredBoundsInst, _.strictnessError(hkparam -> hkarg))
          else kindCheck(declaredBoundsInst <:< argumentBounds, _.strictnessError(hkarg -> hkparam))

          debuglog(
            "checkKindBoundsHK base case: " + hkparam +
            " declared bounds: " + declaredBounds +
            " after instantiating earlier hkparams: " + declaredBoundsInst + "\n" +
            "checkKindBoundsHK base case: "+ hkarg +
            " has bounds: " + argumentBounds
          )
        }
        else {
          hkarg.initialize // scala/bug#7902 otherwise hkarg.typeParams yields List(NoSymbol)!
          debuglog("checkKindBoundsHK recursing to compare params of "+ hkparam +" with "+ hkarg)
          kindErrors ++= checkKindBoundsHK(
            hkarg.typeParams,
            hkarg,
            argPre,
            argOwner,
            hkparam,
            paramOwner,
            underHKParams ++ hkparam.typeParams,
            withHKArgs ++ hkarg.typeParams,
            !flip
          )
        }
        if (!explainErrors && !kindErrors.isEmpty)
          return kindErrors
      }
      if (explainErrors) kindErrors
      else NoKindErrors
    }

    if (settings.isDebug && (tparams.nonEmpty || targs.nonEmpty)) log(
      "checkKindBounds0(" + tparams + ", " + targs + ", " + pre + ", "
      + owner + ", " + explainErrors + ")"
    )

    flatMap2(tparams, targs) { (tparam, targ) =>
      // Prevent WildcardType from causing kind errors, as typevars may be higher-order
      if (targ == WildcardType) Nil else {
        // NOTE: *not* targ.typeSymbol, which normalizes
        // force initialize symbol for scala/bug#4205
        val targSym = targ.typeSymbolDirect.initialize
        // NOTE: *not* targ.prefix, which normalizes
        val targPre = targ.prefixDirect
        // @M must use the typeParams of the *type* targ, not of the *symbol* of targ!!
        val tparamsHO = targ.typeParams
        if (targ.isHigherKinded || tparam.typeParams.nonEmpty) {
          val kindErrors = checkKindBoundsHK(
            tparamsHO, targSym, targPre, targSym.owner,
            tparam, tparam.owner, tparam.typeParams, tparamsHO,
            flip = false
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

  /**
   * The data structure describing the kind of a given type.
   *
   * Proper types are represented using ProperTypeKind.
   *
   * Type constructors are represented using TypeConKind.
   */
  abstract class Kind {
    import Kind.StringState
    def description: String
    def order: Int
    def bounds: TypeBounds

    /** Scala syntax notation of this kind.
     * Proper types are expresses as A.
     * Type constructors are expressed as F[k1 >: lo <: hi, k2, ...] where k1, k2, ... are parameter kinds.
     * If the bounds exists at any level, it preserves the type variable names. Otherwise,
     * it uses prescribed letters for each level: A, F, X, Y, Z.
     */
    def scalaNotation: String

    /** Kind notation used in https://adriaanm.github.com/files/higher.pdf.
     * Proper types are expressed as *.
     * Type constructors are expressed * -> *(lo, hi) -(+)-> *.
     */
    def starNotation: String

    /** Contains bounds either as part of itself or its arguments.
     */
    def hasBounds: Boolean = !bounds.isEmptyBounds

    private[internal] def buildState(sym: Symbol, v: Variance)(s: StringState): StringState
  }
  object Kind {
    private[internal] sealed trait ScalaNotation
    private[internal] sealed case class Head(order: Int, n: Option[Int], alias: Option[String]) extends ScalaNotation {
      override def toString: String = {
        alias getOrElse {
          typeAlias(order) + n.map(_.toString).getOrElse("")
        }
      }
      private def typeAlias(x: Int): String =
        x match {
          case 0 => "A"
          case 1 => "F"
          case 2 => "X"
          case 3 => "Y"
          case 4 => "Z"
          case x if x < 12 => ('O'.toInt - 5 + x).toChar.toString
          case _ => "V"
        }
    }
    private[internal] sealed case class Text(value: String) extends ScalaNotation {
      override def toString: String = value
    }
    private[internal] case class StringState(tokens: Seq[ScalaNotation]) {
      override def toString: String = tokens.mkString
      def append(value: String): StringState = StringState(tokens :+ Text(value))
      def appendHead(order: Int, sym: Symbol): StringState = {
        val n = countByOrder(order) + 1
        val alias = if (sym eq NoSymbol) None
                    else Some(sym.nameString)
        StringState(tokens :+ Head(order, Some(n), alias))
      }
      def countByOrder(o: Int): Int = tokens count {
        case Head(`o`, _, _) => true
        case t               => false
      }
      // Replace Head(o, Some(1), a) with Head(o, None, a) if countByOrder(o) <= 1, so F1[A] becomes F[A]
      def removeOnes: StringState = {
        val maxOrder = (tokens map {
          case Head(o, _, _) => o
          case _             => 0
        }).max
        StringState((0 to maxOrder).foldLeft(tokens){ (ts: Seq[ScalaNotation], o: Int) =>
          if (countByOrder(o) <= 1)
            ts map {
              case Head(`o`, _, a) => Head(o, None, a)
              case t               => t
            }
          else ts
        })
      }
      // Replace Head(o, n, Some(_)) with Head(o, n, None), so F[F] becomes F[A].
      def removeAlias: StringState = {
        StringState(tokens map {
          case Head(o, n, Some(_)) => Head(o, n, None)
          case t                   => t
        })
      }
    }
    private[internal] object StringState {
      def empty: StringState = StringState(Seq())
    }
    def FromParams(tparams: List[Symbol]): Type = GenPolyType(tparams, AnyTpe)
    def Wildcard: Type                          = WildcardType
  }
  class ProperTypeKind(val bounds: TypeBounds) extends Kind {
    import Kind.StringState
    val description: String = "This is a proper type."
    val order = 0
    private[internal] def buildState(sym: Symbol, v: Variance)(s: StringState): StringState = {
      s.append(v.symbolicString).appendHead(order, sym).append(bounds.scalaNotation(_.toString))
    }
    def scalaNotation: String = Kind.Head(order, None, None).toString + bounds.scalaNotation(_.toString)
    def starNotation: String = "*" + bounds.starNotation(_.toString)
  }
  object ProperTypeKind {
    def apply: ProperTypeKind = this(TypeBounds.empty)
    def apply(bounds: TypeBounds): ProperTypeKind = new ProperTypeKind(bounds)
    def unapply(ptk: ProperTypeKind): Some[TypeBounds] = Some(ptk.bounds)
  }

  class TypeConKind(val bounds: TypeBounds, val args: Seq[TypeConKind.Argument]) extends Kind {
    import Kind.StringState
    val order = (args map (_.kind.order)).max + 1
    def description: String =
      if (order == 1) "This is a type constructor: a 1st-order-kinded type."
      else  "This is a type constructor that takes type constructor(s): a higher-kinded type."
    override def hasBounds: Boolean = super.hasBounds || args.exists(_.kind.hasBounds)
    def scalaNotation: String = {
      val s = buildState(NoSymbol, Variance.Invariant)(StringState.empty).removeOnes
      val s2 = if (hasBounds) s
               else s.removeAlias
      s2.toString
    }
    private[internal] def buildState(sym: Symbol, v: Variance)(s0: StringState): StringState = {
      var s: StringState = s0
      s = s.append(v.symbolicString).appendHead(order, sym).append("[")
      args.zipWithIndex foreach { case (arg, i) =>
        s = arg.kind.buildState(arg.sym, arg.variance)(s)
        if (i != args.size - 1) {
          s = s.append(",")
        }
      }
      s = s.append("]").append(bounds.scalaNotation(_.toString))
      s
    }
    def starNotation: String = {
      import Variance._
      (args map { arg =>
        (if (arg.kind.order == 0) arg.kind.starNotation
        else "(" + arg.kind.starNotation + ")") +
        (if (arg.variance == Invariant) " -> "
        else " -(" + arg.variance.symbolicString + ")-> ")
      }).mkString + "*" + bounds.starNotation(_.toString)
    }
  }
  object TypeConKind {
    def apply(args: Seq[TypeConKind.Argument]): TypeConKind = this(TypeBounds.empty, args)
    def apply(bounds: TypeBounds, args: Seq[TypeConKind.Argument]): TypeConKind = new TypeConKind(bounds, args)
    def unapply(tck: TypeConKind): Some[(TypeBounds, Seq[TypeConKind.Argument])] = Some((tck.bounds, tck.args))
    case class Argument(variance: Variance, kind: Kind)(val sym: Symbol) {}
  }

  /**
   * Starting from a Symbol (sym) or a Type (tpe), infer the kind that classifies it (sym.tpeHK/tpe).
   */
  object inferKind {
    import TypeConKind.Argument

    abstract class InferKind {
      protected def infer(tpe: Type, owner: Symbol, topLevel: Boolean): Kind
      protected def infer(sym: Symbol, topLevel: Boolean): Kind = infer(sym.tpeHK, sym.owner, topLevel)
      def apply(sym: Symbol): Kind = infer(sym, topLevel = true)
      def apply(tpe: Type, owner: Symbol): Kind = infer(tpe, owner, topLevel = true)
    }

    def apply(pre: Type): InferKind = new InferKind {
      protected def infer(tpe: Type, owner: Symbol, topLevel: Boolean): Kind = {
        val bounds = if (topLevel) TypeBounds.empty
                     else tpe.asSeenFrom(pre, owner).bounds
        if(!tpe.isHigherKinded) ProperTypeKind(bounds)
        else TypeConKind(bounds, tpe.typeParams map { p => Argument(p.variance, infer(p, topLevel = false))(p) })
      }
    }
  }
}
