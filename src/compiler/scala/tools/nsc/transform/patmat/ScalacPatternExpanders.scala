/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools
package nsc
package transform
package patmat

/** This is scalac-specific logic layered on top of the scalac-agnostic
 *  "matching products to patterns" logic defined in PatternExpander.
 */
trait ScalacPatternExpanders {
  val global: Global

  import global._
  import definitions._
  import treeInfo._
  import analyzer._

  type PatternAligned = ScalacPatternExpander#Aligned

  implicit class AlignedOps(val aligned: PatternAligned) {
    import aligned._
    def expectedTypes     = typedPatterns map (_.tpe)
    def unexpandedFormals = extractor.varargsTypes
  }
  trait ScalacPatternExpander extends PatternExpander[Tree, Type] {
    def NoPattern = EmptyTree
    def NoType    = global.NoType

    def newPatterns(patterns: List[Tree]): Patterns = patterns match {
      case init :+ last if isStar(last) => Patterns(init, last)
      case _                            => Patterns(patterns, NoPattern)
    }
    def elementTypeOf(tpe: Type) = {
      val seq = repeatedToSeq(tpe)

      ( typeOfMemberNamedHead(seq)
          orElse typeOfMemberNamedApply(seq)
          orElse definitions.elementType(ArrayClass, seq)
      )
    }
    def newExtractor(whole: Type, fixed: List[Type], repeated: Repeated, typeOfSinglePattern: Type): Extractor =
      logResult(s"newExtractor($whole, $fixed, $repeated, $typeOfSinglePattern")(Extractor(whole, fixed, repeated, typeOfSinglePattern))
    def newExtractor(whole: Type, fixed: List[Type], repeated: Repeated): Extractor = newExtractor(whole, fixed, repeated, tupleType(fixed))

    // Turn Seq[A] into Repeated(Seq[A], A, A*)
    def repeatedFromSeq(seqType: Type): Repeated = {
      val elem     = elementTypeOf(seqType)
      val repeated = scalaRepeatedType(elem)

      Repeated(seqType, elem, repeated)
    }
    // Turn A* into Repeated(Seq[A], A, A*)
    def repeatedFromVarargs(repeated: Type): Repeated =
      Repeated(repeatedToSeq(repeated), repeatedToSingle(repeated), repeated)

    /** In this case we are basing the pattern expansion on a case class constructor.
     *  The argument is the MethodType carried by the primary constructor.
     */
    def applyMethodTypes(method: Type): Extractor = {
      val whole = method.finalResultType

      method.paramTypes match {
        case init :+ last if isScalaRepeatedParamType(last) => newExtractor(whole, init, repeatedFromVarargs(last))
        case tps                                            => newExtractor(whole, tps, NoRepeated)
      }
    }

    /** In this case, expansion is based on an unapply or unapplySeq method.
     *  Unfortunately the MethodType does not carry the information of whether
     *  it was unapplySeq, so we have to funnel that information in separately.
     */
    def unapplyMethodTypes(context: Context, whole: Type, result: Type, isSeq: Boolean): Extractor = {
      if (result =:= BooleanTpe) newExtractor(whole, Nil, NoRepeated)
      else {
        val getResult = typeOfMemberNamedGet(result)
        def noGetError() = {
          val name = "unapply" + (if (isSeq) "Seq" else "")
          context.error(context.tree.pos, s"The result type of an $name method must contain a member `get` to be used as an extractor pattern, no such member exists in ${result}")
        }
        val expanded = getResult match {
          case global.NoType => noGetError(); Nil
          case rawGet if !hasSelectors(rawGet) => rawGet :: Nil
          case rawGet                          => typesOfSelectors(rawGet)
        }
        expanded match {
          case init :+ last if isSeq => newExtractor(whole, init, repeatedFromSeq(last), getResult)
          case tps                   => newExtractor(whole, tps, NoRepeated, getResult)
        }
      }
    }
  }
  object alignPatterns extends ScalacPatternExpander {
    private def validateAligned(context: Context, tree: Tree, aligned: Aligned): Aligned = {
      import aligned._

      def owner         = tree.symbol.owner
      def offering      = extractor.offeringString
      def symString     = tree.symbol.fullLocationString
      def offerString   = if (extractor.isErroneous) "" else s" offering $offering"
      def arityExpected = ( if (extractor.hasSeq) "at least " else "" ) + productArity

      def err(msg: String)         = context.error(tree.pos, msg)
      def warn(msg: String)        = context.warning(tree.pos, msg)
      def arityError(what: String) = err(s"$what patterns for $owner$offerString: expected $arityExpected, found $totalArity")

      if (isStar && !isSeq)
        err("Star pattern must correspond with varargs or unapplySeq")
      else if (elementArity < 0)
        arityError("not enough")
      else if (elementArity > 0 && !isSeq)
        arityError("too many")
      else if (settings.warnStarsAlign && isSeq && productArity > 0 && elementArity > 0) warn {
        if (isStar) "Sequence wildcard (_*) does not align with repeated case parameter or extracted sequence; the result may be unexpected."
        else "A repeated case parameter or extracted sequence is not matched by a sequence wildcard (_*), and may fail at runtime."
      }

      aligned
    }

    def apply(context: Context, sel: Tree, args: List[Tree]): Aligned = {
      val fn = sel match {
        case Unapplied(fn) => fn
        case _             => sel
      }
      val patterns  = newPatterns(args)
      val isUnapply = sel.symbol.name == nme.unapply

      val extractor = sel.symbol.name match {
        case nme.unapply    => unapplyMethodTypes(context, firstParamType(fn.tpe), sel.tpe, isSeq = false)
        case nme.unapplySeq => unapplyMethodTypes(context, firstParamType(fn.tpe), sel.tpe, isSeq = true)
        case _              => applyMethodTypes(fn.tpe)
      }

      /** Rather than let the error that is SI-6675 pollute the entire matching
       *  process, we will tuple the extractor before creation Aligned so that
       *  it contains known good values.
       */
      def productArity    = extractor.productArity
      def acceptMessage   = if (extractor.isErroneous) "" else s" to hold ${extractor.offeringString}"
      val requiresTupling = isUnapply && patterns.totalArity == 1 && productArity > 1

      val normalizedExtractor = if (requiresTupling) {
        val tupled = extractor.asSinglePattern
        if (effectivePatternArity(args) == 1 && isTupleType(extractor.typeOfSinglePattern)) {
          val sym = sel.symbol.owner
          currentRun.reporting.deprecationWarning(sel.pos, sym, s"${sym} expects $productArity patterns$acceptMessage but crushing into $productArity-tuple to fit single pattern (SI-6675)", "2.11.0")
        }
        tupled
      } else extractor
      validateAligned(context, fn, Aligned(patterns, normalizedExtractor))
    }

    def apply(context: Context, tree: Tree): Aligned = tree match {
      case Apply(fn, args)   => apply(context, fn, args)
      case UnApply(fn, args) => apply(context, fn, args)
    }
  }
}
