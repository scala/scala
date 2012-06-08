/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */

package scala.tools.nsc
package matching

import PartialFunction._
import scala.collection.{ mutable }
import scala.reflect.internal.util.Position
import transform.ExplicitOuter
import symtab.Flags
import mutable.ListBuffer
import annotation.elidable
import language.postfixOps

trait ParallelMatching extends ast.TreeDSL
      with MatchSupport
      with Matrix
      with Patterns
      with PatternBindings
{
  self: ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions.{
    AnyRefClass, IntClass, BooleanClass, SomeClass, OptionClass,
    getProductArgs, productProj, Object_eq, Any_asInstanceOf
  }
  import CODE._
  import Types._
  import Debug._

  /** Transition **/
  def toPats(xs: List[Tree]): List[Pattern] = xs map Pattern.apply

  /** The umbrella matrix class. **/
  abstract class MatchMatrix(val context: MatrixContext) extends MatchMatrixOptimizer with MatrixExhaustiveness {
    import context._

    def data: MatrixContext#MatrixInit

    lazy val MatrixInit(roots, cases, failTree) = data
    lazy val (rows, targets)                    = expand(roots, cases).unzip
    lazy val expansion: Rep                     = make(roots, rows)

    private val shortCuts = perRunCaches.newMap[Int, Symbol]()

    final def createShortCut(theLabel: Symbol): Int = {
      val key = shortCuts.size + 1
      shortCuts(key) = theLabel
      -key
    }
    def createLabelDef(namePrefix: String, body: Tree, params: List[Symbol] = Nil, restpe: Type = matchResultType) = {
      val labelName = cunit.freshTermName(namePrefix)
      val labelSym  = owner.newLabel(labelName, owner.pos)
      val labelInfo = MethodType(params, restpe)

      LabelDef(labelSym setInfo labelInfo, params, body setType restpe)
    }

    /** This is the recursively focal point for translating the current
     *  list of pattern variables and a list of pattern match rows into
     *  a tree suitable for entering erasure.
     *
     *  The first time it is called, the variables are (copies of) the
     *  original pattern matcher roots, and the rows correspond to the
     *  original casedefs.
     */
    final def make(roots1: PatternVarGroup, rows1: List[Row]): Rep = {
      traceCategory("New Match", "%sx%s (%s)", roots1.size, rows1.size, roots1.syms.mkString(", "))
      def classifyPat(opat: Pattern, j: Int): Pattern = opat simplify roots1(j)

      val newRows = rows1 flatMap (_ expandAlternatives classifyPat)
      if (rows1.length != newRows.length) make(roots1, newRows)  // recursive call if any change
      else {
        val rep = Rep(roots1, newRows)
        new ExhaustivenessChecker(rep, roots.head.sym.pos).check
        rep
      }
    }

    override def toString() = "MatchMatrix(%s) { %s }".format(matchResultType, indentAll(targets))

    /**
     * Encapsulates a symbol being matched on.  It is created from a
     * PatternVar, which encapsulates the symbol's creation and assignment.
     *
     * We never match on trees directly - a temporary variable is created
     * (in a PatternVar) for any expression being matched on.
     */
    class Scrutinee(val pv: PatternVar) {
      import definitions._

      // presenting a face of our symbol
      def sym   = pv.sym
      def tpe   = sym.tpe
      def pos   = sym.pos
      def id    = ID(sym) setPos pos  // attributed ident

      def accessors     = if (isCaseClass) sym.caseFieldAccessors else Nil
      def accessorTypes = accessors map (x => (tpe memberType x).resultType)

      lazy val accessorPatternVars  = PatternVarGroup(
        for ((accessor, tpe) <- accessors zip accessorTypes) yield
          createVar(tpe, _ => fn(id, accessor))
      )

      private def extraValDefs = if (pv.rhs.isEmpty) Nil else List(pv.valDef)
      def allValDefs = extraValDefs ::: accessorPatternVars.valDefs

      // tests
      def isDefined      = sym ne NoSymbol
      def isSubrangeType = subrangeTypes(tpe.typeSymbol)
      def isCaseClass    = tpe.typeSymbol.isCase

      // sequences
      def seqType         = tpe.widen baseType SeqClass
      def elemType        = tpe typeArgs 0

      private def elemAt(i: Int)  = (id DOT (tpe member nme.apply))(LIT(i))
      private def createElemVar(i: Int)   = createVar(elemType, _ => elemAt(i))
      private def createSeqVar(drop: Int) = createVar(seqType, _ => id DROP drop)

      def createSequenceVars(count: Int): List[PatternVar] =
        (0 to count).toList map (i => if (i < count) createElemVar(i) else createSeqVar(i))

      // for propagating "unchecked" to synthetic vars
      def isChecked = !(sym hasFlag NO_EXHAUSTIVE)
      def flags: List[Long] = List(NO_EXHAUSTIVE) filter (sym hasFlag _)

      // this is probably where this actually belongs
      def createVar(tpe: Type, f: Symbol => Tree) = context.createVar(tpe, f, isChecked)

      def castedTo(headType: Type) =
        if (tpe =:= headType) this
        else new Scrutinee(createVar(headType, lhs => gen.mkAsInstanceOf(id, lhs.tpe)))

      override def toString() = "(%s: %s)".format(id, tpe)
    }

    def isPatternSwitch(scrut: Scrutinee, ps: List[Pattern]): Option[PatternSwitch] = {
      def isSwitchableConst(x: Pattern) = cond(x) { case x: LiteralPattern if x.isSwitchable => true }
      def isSwitchableDefault(x: Pattern) = isSwitchableConst(x) || x.isDefault

      // TODO - scala> (5: Any) match { case 5 => 5 ; case 6 => 7 }
      // ... should compile to a switch.  It doesn't because the scrut isn't Int/Char, but
      // that could be handle in an if/else since every pattern requires an Int.
      // More immediately, Byte and Short scruts should also work.
      if (!scrut.isSubrangeType) None
      else {
        val (_lits, others) = ps span isSwitchableConst
        val lits = _lits collect { case x: LiteralPattern => x }

        condOpt(others) {
          case Nil                                => new PatternSwitch(scrut, lits, None)
          // TODO: This needs to also allow the case that the last is a compatible type pattern.
          case List(x) if isSwitchableDefault(x)  => new PatternSwitch(scrut, lits, Some(x))
        }
      }
    }

    class PatternSwitch(
      scrut: Scrutinee,
      override val ps: List[LiteralPattern],
      val defaultPattern: Option[Pattern]
    ) extends PatternMatch(scrut, ps) {
      require(scrut.isSubrangeType && (ps forall (_.isSwitchable)))
    }

    case class PatternMatch(scrut: Scrutinee, ps: List[Pattern]) {
      def head = ps.head
      def tail = ps.tail
      def size = ps.length

      def headType = head.necessaryType
      private val dummyCount = if (head.isCaseClass) headType.typeSymbol.caseFieldAccessors.length else 0
      def dummies = emptyPatterns(dummyCount)

      def apply(i: Int): Pattern = ps(i)
      def pzip() = ps.zipWithIndex
      def pzip[T](others: List[T]) = {
        assert(ps.size == others.size, "Internal error: ps = %s, others = %s".format(ps, others))
        ps zip others
      }

      // Any unapply - returns Some(true) if a type test is needed before the unapply can
      // be called (e.g. def unapply(x: Foo) = { ... } but our scrutinee is type Any.)
      object AnyUnapply {
        def unapply(x: Pattern): Option[Boolean] = condOpt(x.tree) {
          case UnapplyParamType(tpe) => !(scrut.tpe <:< tpe)
        }
      }

      def mkRule(rest: Rep): RuleApplication = {
        tracing("Rule")(head match {
          case x if isEquals(x.tree.tpe)        => new MixEquals(this, rest)
          case x: SequencePattern               => new MixSequence(this, rest, x)
          case AnyUnapply(false)                => new MixUnapply(this, rest)
          case _ =>
            isPatternSwitch(scrut, ps) match {
              case Some(x)  => new MixLiteralInts(x, rest)
              case _        => new MixTypes(this, rest)
            }
        })
      }
      override def toString() = "%s match {%s}".format(scrut, indentAll(ps))
    } // PatternMatch

    /***** Rule Applications *****/

    sealed abstract class RuleApplication {
      def pmatch: PatternMatch
      def rest: Rep
      def cond: Tree
      def success: Tree
      def failure: Tree

      lazy val PatternMatch(scrut, patterns) = pmatch
      lazy val head = pmatch.head
      lazy val codegen: Tree = IF (cond) THEN (success) ELSE (failure)

      def mkFail(xs: List[Row]): Tree =
        if (xs.isEmpty) failTree
        else remake(xs).toTree

      def remake(
        rows: List[Row],
        pvgroup: PatternVarGroup = emptyPatternVarGroup,
        includeScrut: Boolean = true): Rep =
      {
        val scrutpvs = if (includeScrut) List(scrut.pv) else Nil
        make(pvgroup.pvs ::: scrutpvs ::: rest.tvars, rows)
      }

      /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
      def tree(): Tree

      override def toString =
        "Rule/%s (%s =^= %s)".format(getClass.getSimpleName, scrut, head)
    }

    /** {case ... if guard => bx} else {guardedRest} */
    /** VariableRule: The top-most rows has only variable (non-constructor) patterns. */
    case class VariableRule(subst: Bindings, guard: Tree, guardedRest: Rep, bx: Int) extends RuleApplication {
      def pmatch: PatternMatch = impossible
      def rest: Rep = guardedRest

      private lazy val (valDefs, successTree) = targets(bx) applyBindings subst.toMap
      lazy val cond    = guard
      lazy val success = successTree
      lazy val failure = guardedRest.toTree

      final def tree(): Tree =
        if (bx < 0) REF(shortCuts(-bx))
        else squeezedBlock(
          valDefs,
          if (cond.isEmpty) success else codegen
        )

      override def toString = "(case %d) {\n  Bindings: %s\n\n  if (%s) { %s }\n  else { %s }\n}".format(
        bx, subst, guard, success, guardedRest
      )
    }

    class MixLiteralInts(val pmatch: PatternSwitch, val rest: Rep) extends RuleApplication {
      val literals = pmatch.ps
      val defaultPattern = pmatch.defaultPattern

      private lazy val casted: Tree =
        if (!scrut.tpe.isInt) scrut.id DOT nme.toInt else scrut.id

      // creates a row transformer for injecting the default case bindings at a given index
      private def addDefaultVars(index: Int): Row => Row =
        if (defaultVars.isEmpty) identity
        else rebindAll(_, pmatch(index).boundVariables, scrut.sym)

      // add bindings for all the given vs to the given tvar
      private def rebindAll(r: Row, vs: Iterable[Symbol], tvar: Symbol) =
        r rebind r.subst.add(vs, tvar)

      private def bindVars(Tag: Int, orig: Bindings): Bindings = {
        def myBindVars(rest: List[(Int, List[Symbol])], bnd: Bindings): Bindings = rest match {
          case Nil => bnd
          case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs, scrut.sym))
          case (_,  vs)::xs => myBindVars(xs, bnd)
        }
        myBindVars(varMap, orig)
      }

      // bound vars and rows for default pattern (only one row, but a list is easier to use later)
      lazy val (defaultVars, defaultRows) = defaultPattern match {
        case None    => (Nil, Nil)
        case Some(p) => (p.boundVariables, List(rebindAll(rest rows literals.size, p.boundVariables, scrut.sym)))
      }

      // literalMap is a map from each literal to a list of row indices.
      // varMap is a list from each literal to a list of the defined vars.
      lazy val (litPairs, varMap) = (
        literals.zipWithIndex map {
          case (lit, index) =>
            val tag  = lit.intValue
            (tag -> index, tag -> lit.boundVariables)
        } unzip
      )
      def literalMap = litPairs groupBy (_._1) map {
        case (k, vs) => (k, vs map (_._2))
      }

      lazy val cases =
        for ((tag, indices) <- literalMap.toList.sortBy(_._1)) yield {
          val newRows = indices map (i => addDefaultVars(i)(rest rows i))
          val r       = remake(newRows ++ defaultRows, includeScrut = false)
          val r2      = make(r.tvars, r.rows map (x => x rebind bindVars(tag, x.subst)))

          CASE(Literal(Constant(tag))) ==> r2.toTree
        }

      lazy val defaultTree = remake(defaultRows, includeScrut = false).toTree
      def defaultCase = CASE(WILD(IntClass.tpe)) ==> defaultTree

      // cond/success/failure only used if there is exactly one case.
      lazy val cond    = scrut.id MEMBER_== cases.head.pat
      lazy val success = cases.head.body
      lazy val failure = defaultTree

      // only one case becomes if/else, otherwise match
      def tree() =
        if (cases.size == 1) codegen
        else casted MATCH (cases :+ defaultCase: _*)
    }

    /** mixture rule for unapply pattern
     */
    class MixUnapply(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      val Pattern(UnApply(unMethod, unArgs)) = head
      val Apply(unTarget, _ :: trailing) = unMethod

      object SameUnapplyCall {
        def isSame(t: Tree) = isEquivalentTree(unTarget, t)
        def unapply(x: Pattern) = /*tracing("SameUnapplyCall (%s vs. %s)".format(unTarget, x))*/(x match {
          case Pattern(UnApply(Apply(fn, _), args)) if isSame(fn) => Some(args)
          case _                                                  => None
        })
      }
      object SameUnapplyPattern {
        def isSame(t: Tree)   = isEquivalentTree(unMethod, t)
        def apply(x: Pattern) = unapply(x).isDefined
        def unapply(x: Pattern) = /*tracing("SameUnapplyPattern (%s vs. %s)".format(unMethod, x))*/(x match {
          case Pattern(UnApply(t, _)) if isSame(t) => Some(unArgs)
          case _                                   => None
        })
      }

      private lazy val zipped      = pmatch pzip rest.rows

      lazy val unapplyResult: PatternVar =
        scrut.createVar(unMethod.tpe, Apply(unTarget, scrut.id :: trailing) setType _.tpe)

      lazy val cond: Tree = unapplyResult.tpe.normalize match {
        case TypeRef(_, BooleanClass, _)  => unapplyResult.ident
        case TypeRef(_, SomeClass, _)     => TRUE
        case _                            => NOT(unapplyResult.ident DOT nme.isEmpty)
      }

      lazy val failure =
        mkFail(zipped.tail filterNot (x => SameUnapplyPattern(x._1)) map { case (pat, r) => r insert pat })

      private def doSuccess: (List[PatternVar], List[PatternVar], List[Row]) = {
        // pattern variable for the unapply result of Some(x).get
        def unMethodTypeArg = unMethod.tpe.baseType(OptionClass).typeArgs match {
          case Nil      => log("No type argument for unapply result! " + unMethod.tpe) ; NoType
          case arg :: _ => arg
        }
        lazy val pv = scrut.createVar(unMethodTypeArg, _ => fn(ID(unapplyResult.lhs), nme.get))
        def tuple = pv.lhs

        // at this point it's Some[T1,T2...]
        lazy val tpes  = getProductArgs(tuple.tpe)

        // one pattern variable per tuple element
        lazy val tuplePVs =
          for ((tpe, i) <- tpes.zipWithIndex) yield
            scrut.createVar(tpe, _ => fn(ID(tuple), productProj(tuple, i + 1)))

        // the filter prevents infinite unapply recursion
        def mkNewRows(sameFilter: (List[Tree]) => List[Tree]) = {
          val dum = if (unArgs.length <= 1) unArgs.length else tpes.size
          for ((pat, r) <- zipped) yield pat match {
            case SameUnapplyCall(xs)  => r.insert2(toPats(sameFilter(xs)) :+ NoPattern, pat.boundVariables, scrut.sym)
            case _                    => r insert (emptyPatterns(dum) :+ pat)
          }
        }

        // 0 is Boolean, 1 is Option[T], 2+ is Option[(T1,T2,...)]
        unArgs.length match {
          case 0  => (Nil, Nil, mkNewRows((xs) => Nil))
          case 1  => (List(pv), List(pv), mkNewRows(xs => List(xs.head)))
          case _  => (pv :: tuplePVs, tuplePVs, mkNewRows(identity))
        }
      }

      lazy val success = {
        val (squeezePVs, pvs, rows) = doSuccess
        val srep = remake(rows, pvs).toTree

        squeezedBlock(squeezePVs map (_.valDef), srep)
      }

      final def tree() =
        squeezedBlock(List(handleOuter(unapplyResult.valDef)), codegen)
    }

    /** Handle Sequence patterns (including Star patterns.)
     *  Note: pivot == head, just better typed.
     */
    sealed class MixSequence(val pmatch: PatternMatch, val rest: Rep, pivot: SequencePattern) extends RuleApplication {
      require(scrut.tpe <:< head.tpe)

      def hasStar = pivot.hasStar
      private def pivotLen    = pivot.nonStarLength
      private def seqDummies  = emptyPatterns(pivot.elems.length + 1)

      // Should the given pattern join the expanded pivot in the success matrix? If so,
      // this partial function will be defined for the pattern, and the result of the apply
      // is the expanded sequence of new patterns.
      lazy val successMatrixFn = new PartialFunction[Pattern, List[Pattern]] {
        private def seqIsDefinedAt(x: SequenceLikePattern) = (hasStar, x.hasStar) match {
          case (true, true)   => true
          case (true, false)  => pivotLen <= x.nonStarLength
          case (false, true)  => pivotLen >= x.nonStarLength
          case (false, false) => pivotLen == x.nonStarLength
        }

        def isDefinedAt(pat: Pattern) = pat match {
          case x: SequenceLikePattern => seqIsDefinedAt(x)
          case WildcardPattern()      => true
          case _                      => false
        }

        def apply(pat: Pattern): List[Pattern] = pat match {
          case x: SequenceLikePattern =>
            def isSameLength  = pivotLen == x.nonStarLength
            def rebound       = x.nonStarPatterns :+ (x.elemPatterns.last rebindTo WILD(scrut.seqType))

            (pivot.hasStar, x.hasStar, isSameLength) match {
              case (true, true, true)   => rebound :+ NoPattern
              case (true, true, false)  => (seqDummies drop 1) :+ x
              case (true, false, true)  => x.elemPatterns ++ List(NilPattern, NoPattern)
              case (false, true, true)  => rebound
              case (false, false, true) => x.elemPatterns :+ NoPattern
              case _                    => seqDummies
            }

          case _  => seqDummies
        }
      }

      // Should the given pattern be in the fail matrix? This is true of any sequences
      // as long as the result of the length test on the pivot doesn't make it impossible:
      // for instance if neither sequence is right ignoring and they are of different
      // lengths, the later one cannot match since its length must be wrong.
      def failureMatrixFn(c: Pattern) = (pivot ne c) && (c match {
        case x: SequenceLikePattern =>
          (hasStar, x.hasStar) match {
            case (_, true)      => true
            case (true, false)  => pivotLen > x.nonStarLength
            case (false, false) => pivotLen != x.nonStarLength
          }
        case WildcardPattern()      => true
        case _                      => false
      })

      // divide the remaining rows into success/failure branches, expanding subsequences of patterns
      val successRows = pmatch pzip rest.rows collect {
        case (c, row) if successMatrixFn isDefinedAt c => row insert successMatrixFn(c)
      }
      val failRows = pmatch pzip rest.rows collect {
        case (c, row) if failureMatrixFn(c) => row insert c
      }

      // the discrimination test for sequences is a call to lengthCompare.  Note that
      // this logic must be fully consistent wiith successMatrixFn and failureMatrixFn above:
      // any inconsistency will (and frequently has) manifested as pattern matcher crashes.
      lazy val cond = {
        // the method call symbol
        val methodOp: Symbol                = head.tpe member nme.lengthCompare

        // the comparison to perform.  If the pivot is right ignoring, then a scrutinee sequence
        // of >= pivot length could match it; otherwise it must be exactly equal.
        val compareOp: (Tree, Tree) => Tree = if (hasStar) _ INT_>= _ else _ INT_== _

        // scrutinee.lengthCompare(pivotLength) [== | >=] 0
        val compareFn: Tree => Tree         = (t: Tree) => compareOp((t DOT methodOp)(LIT(pivotLen)), ZERO)

        // wrapping in a null check on the scrutinee
        // XXX this needs to use the logic in "def condition"
        nullSafe(compareFn, FALSE)(scrut.id)
        // condition(head.tpe, scrut.id, head.boundVariables.nonEmpty)
      }
      lazy val success = {
        // one pattern var per sequence element up to elemCount, and one more for the rest of the sequence
        lazy val pvs = scrut createSequenceVars pivotLen

        squeezedBlock(pvs map (_.valDef), remake(successRows, pvs, hasStar).toTree)
      }
      lazy val failure  = remake(failRows).toTree

      final def tree(): Tree = codegen
    }

    class MixEquals(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      private lazy val rhs =
        decodedEqualsType(head.tpe) match {
          case SingleType(pre, sym) => REF(pre, sym)
          case PseudoType(o)        => o
        }
      private lazy val labelDef =
        createLabelDef("fail%", remake((rest.rows.tail, pmatch.tail).zipped map (_ insert _)).toTree)

      lazy val cond       = handleOuter(rhs MEMBER_== scrut.id)
      lazy val successOne = rest.rows.head.insert2(List(NoPattern), head.boundVariables, scrut.sym)
      lazy val successTwo = Row(emptyPatterns(1 + rest.tvars.size), NoBinding, EmptyTree, createShortCut(labelDef.symbol))
      lazy val success    = remake(List(successOne, successTwo)).toTree
      lazy val failure    = labelDef

      final def tree() = codegen
      override def toString() = "MixEquals(%s == %s)".format(scrut, head)
    }

    /** Mixture rule for type tests.
     *  moreSpecific: more specific patterns
     *      subsumed: more general patterns (subsuming current), rows index and subpatterns
     *     remaining: remaining, rows index and pattern
     */
    class MixTypes(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      case class Yes(bx: Int, moreSpecific: Pattern, subsumed: List[Pattern])
      case class No(bx: Int, remaining: Pattern)

      val (yeses, noes) = {
        val _ys = new ListBuffer[Yes]
        val _ns = new ListBuffer[No]

        for ((pattern, j) <- pmatch.pzip()) {
          // scrutinee, head of pattern group
          val (s, p) = (pattern.tpe, head.necessaryType)

          def isEquivalent  = head.necessaryType =:= pattern.tpe
          def isObjectTest  = pattern.isObject && (p =:= pattern.necessaryType)

          def sMatchesP = matches(s, p)
          def pMatchesS = matches(p, s)

          def ifEquiv(yes: Pattern): Pattern = if (isEquivalent) yes else pattern

          def passl(p: Pattern = NoPattern, ps: List[Pattern] = pmatch.dummies) = Some(Yes(j, p, ps))
          def passr()                                                           = Some( No(j, pattern))

          def typed(pp: Tree) = passl(ifEquiv(Pattern(pp)))
          def subs()          = passl(ifEquiv(NoPattern), pattern subpatterns pmatch)

          val (oneY, oneN) = pattern match {
            case Pattern(LIT(null)) if !(p =:= s)       => (None, passr)      // (1)
            case x if isObjectTest                      => (passl(), None)    // (2)
            case Pattern(Typed(pp, _))     if sMatchesP => (typed(pp), None)  // (4)
            // The next line used to be this which "fixed" 1697 but introduced
            // numerous regressions including #3136.
            // case Pattern(_: UnApply, _)              => (passl(), passr)
            case Pattern(_: UnApply)                    => (None, passr)
            case x if !x.isDefault && sMatchesP         => (subs(), None)
            case x if  x.isDefault || pMatchesS         => (passl(), passr)
            case _                                      => (None, passr)
          }
          oneY map (_ys +=)
          oneN map (_ns +=)
        }
        (_ys.toList, _ns.toList)
      }

      val moreSpecific = yeses map (_.moreSpecific)
      val subsumed = yeses map (x => (x.bx, x.subsumed))
      val remaining = noes map (x => (x.bx, x.remaining))

      private def mkZipped =
        for (Yes(j, moreSpecific, subsumed) <- yeses) yield
          j -> (moreSpecific :: subsumed)

      lazy val casted = scrut castedTo pmatch.headType
      lazy val cond   = condition(casted.tpe, scrut, head.boundVariables.nonEmpty)

      private def isAnyMoreSpecific = yeses exists (x => !x.moreSpecific.isEmpty)
      lazy val (subtests, subtestVars) =
        if (isAnyMoreSpecific)  (mkZipped, List(casted.pv))
        else                    (subsumed, Nil)

      lazy val newRows =
        for ((j, ps) <- subtests) yield
          (rest rows j).insert2(ps, pmatch(j).boundVariables, casted.sym)

      lazy val success = {
        val srep = remake(newRows, subtestVars ::: casted.accessorPatternVars, includeScrut = false)
        squeezedBlock(casted.allValDefs, srep.toTree)
      }

      lazy val failure =
        mkFail(remaining map { case (p1, p2) => rest rows p1 insert p2 })

      final def tree(): Tree = codegen
    }

    /*** States, Rows, Etc. ***/

    case class Row(pats: List[Pattern], subst: Bindings, guard: Tree, bx: Int) {
      private def nobindings = subst.get().isEmpty
      private def bindstr = if (nobindings) "" else pp(subst)

      /** Extracts the 'i'th pattern. */
      def extractColumn(i: Int) = {
        val (x, xs) = extractIndex(pats, i)
        (x, copy(pats = xs))
      }

      /** Replaces the 'i'th pattern with the argument. */
      def replaceAt(i: Int, p: Pattern) = {
        val newps = (pats take i) ::: p :: (pats drop (i + 1))
        copy(pats = newps)
      }

      def insert(h: Pattern)              = copy(pats = h :: pats)
      def insert(hs: List[Pattern])       = copy(pats = hs ::: pats)  // prepends supplied pattern
      def rebind(b: Bindings)             = copy(subst = b)           // substitutes for bindings

      def insert2(hs: List[Pattern], vs: Iterable[Symbol], tvar: Symbol) =
        tracing("insert2")(copy(pats = hs ::: pats, subst = subst.add(vs, tvar)))

      // returns this rows with alternatives expanded
      def expandAlternatives(classifyPat: (Pattern, Int) => Pattern): List[Row] = {
        def isNotAlternative(p: Pattern) = !cond(p.tree) { case _: Alternative => true }

        // classify all the top level patterns - alternatives come back unaltered
        val newPats: List[Pattern] = pats.zipWithIndex map classifyPat.tupled
        // see if any alternatives were in there
        val (ps, others) = newPats span isNotAlternative
        // make a new row for each alternative, with it spliced into the original position
        if (others.isEmpty) List(copy(pats = ps))
        else extractBindings(others.head) map (x => replaceAt(ps.size, x))
      }
      override def toString() = {
        val bs = if (nobindings) "" else "\n" + bindstr
        "Row(%d)(%s%s)".format(bx, pp(pats), bs)
      }
    }
    abstract class State {
      def bx: Int                   // index into the list of rows
      def params: List[Symbol]      // bound names to be supplied as arguments to labeldef
      def body: Tree                // body to execute upon match
      def label: Option[LabelDef]   // label definition for this state

      // Called with a bindings map when a match is achieved.
      // Returns a list of variable declarations based on the labeldef parameters
      // and the given substitution, and the body to execute.
      protected def applyBindingsImpl(subst: Map[Symbol, Symbol]): (List[ValDef], Tree)

      final def applyBindings(subst: Map[Symbol, Symbol]): (List[ValDef], Tree) = {
        _referenceCount += 1
        applyBindingsImpl(subst)
      }

      private var _referenceCount   = 0
      def referenceCount            = _referenceCount
      def unreached                 = referenceCount == 0
      def shouldInline(sym: Symbol) = referenceCount == 1 && label.exists(_.symbol == sym)

      // Creates a simple Ident if the symbol's type conforms to
      // the val definition's type, or a casted Ident if not.
      private def newValIdent(lhs: Symbol, rhs: Symbol) =
        if (rhs.tpe <:< lhs.tpe) Ident(rhs)
        else gen.mkTypeApply(Ident(rhs), Any_asInstanceOf, List(lhs.tpe))

      protected def newValDefinition(lhs: Symbol, rhs: Symbol) =
        typer typedValDef ValDef(lhs, newValIdent(lhs, rhs))

      protected def newValReference(lhs: Symbol, rhs: Symbol) =
        typer typed newValIdent(lhs, rhs)

      protected def valDefsFor(subst: Map[Symbol, Symbol]) = mapSubst(subst)(newValDefinition)
      protected def identsFor(subst: Map[Symbol, Symbol])  = mapSubst(subst)(newValReference)

      protected def mapSubst[T](subst: Map[Symbol, Symbol])(f: (Symbol, Symbol) => T): List[T] =
        params flatMap { lhs =>
          subst get lhs map (rhs => f(lhs, rhs)) orElse {
            // This should not happen; the code should be structured so it is
            // impossible, but that still lies ahead.
            cunit.warning(lhs.pos, "No binding")
            None
          }
        }

      // typer is not able to digest a body of type Nothing being assigned result type Unit
      protected def caseResultType =
        if (body.tpe.isNothing) body.tpe else matchResultType
    }

    case class LiteralState(bx: Int, params: List[Symbol], body: Tree) extends State {
      def label = None

      protected def applyBindingsImpl(subst: Map[Symbol, Symbol]) =
        (valDefsFor(subst), body.duplicate setType caseResultType)
    }

    case class FinalState(bx: Int, params: List[Symbol], body: Tree) extends State {
      traceCategory("Final State", "(%s) => %s", paramsString, body)
      def label = Some(labelDef)

      private lazy val labelDef = createLabelDef("body%" + bx, body, params, caseResultType)

      protected def applyBindingsImpl(subst: Map[Symbol, Symbol]) = {
        val tree =
          if (referenceCount > 1) ID(labelDef.symbol) APPLY identsFor(subst)
          else labelDef

        (valDefsFor(subst), tree)
      }

      private def paramsString = params map (s => s.name + ": " + s.tpe) mkString ", "
      override def toString() = pp("(%s) => %s".format(pp(params), body))
    }

    case class Rep(val tvars: PatternVarGroup, val rows: List[Row]) {
      lazy val Row(pats, subst, guard, index) = rows.head
      lazy val guardedRest        = if (guard.isEmpty) Rep(Nil, Nil) else make(tvars, rows.tail)
      lazy val (defaults, others) = pats span (_.isDefault)

      /** Cut out the column containing the non-default pattern. */
      class Cut(index: Int) {
        /** The first two separate out the 'i'th pattern in each row from the remainder. */
        private val (_column, _rows) = rows map (_ extractColumn index) unzip

        /** Now the 'i'th tvar is separated out and used as a new Scrutinee. */
        private val (_pv, _tvars) = tvars extractIndex index

        /** The non-default pattern (others.head) replaces the column head. */
        private val (_ncol, _nrep) =
          (others.head :: _column.tail, make(_tvars, _rows))

        def mix() = {
          val newScrut = new Scrutinee(new PatternVar(_pv.sym, EmptyTree, _pv.checked))
          PatternMatch(newScrut, _ncol) mkRule _nrep
        }
      }

      /** Converts this to a tree - recursively acquires subreps. */
      final def toTree(): Tree = tracing("toTree")(typer typed applyRule())

      /** The VariableRule. */
      private def variable() = {
        val binding = (defaults map (_.boundVariables) zip tvars.pvs) .
          foldLeft(subst)((b, pair) => b.add(pair._1, pair._2.lhs))

        VariableRule(binding, guard, guardedRest, index)
      }
      /** The MixtureRule: picks a rewrite rule to apply. */
      private def mixture() = new Cut(defaults.size) mix()

      /** Applying the rule will result in one of:
        *
        *   VariableRule - if all patterns are default patterns
        *    MixtureRule - if one or more patterns are not default patterns
        *          Error - no rows remaining
        */
      final def applyRule(): Tree =
        if (rows.isEmpty) failTree
        else if (others.isEmpty) variable.tree()
        else mixture.tree()

      def ppn(x: Any) = pp(x, newlines = true)
      override def toString() =
        if (tvars.isEmpty) "Rep(%d) = %s".format(rows.size, ppn(rows))
        else "Rep(%dx%d)%s%s".format(tvars.size, rows.size, ppn(tvars), ppn(rows))
    }

    /** Expands the patterns recursively. */
    final def expand(roots: List[PatternVar], cases: List[CaseDef]) = tracing("expand") {
      for ((CaseDef(pat, guard, body), bx) <- cases.zipWithIndex) yield {
        val subtrees = pat match {
          case x if roots.length <= 1 => List(x)
          case Apply(_, args)         => args
          case WILD()                 => emptyTrees(roots.length)
        }
        val params = pat filter (_.isInstanceOf[Bind]) map (_.symbol) distinct
        val row    = Row(toPats(subtrees), NoBinding, guard, bx)
        val state  = body match {
          case x: Literal => LiteralState(bx, params, body)
          case _          => FinalState(bx, params, body)
        }

        row -> state
      }
    }

    /** returns the condition in "if (cond) k1 else k2"
     */
    final def condition(tpe: Type, scrut: Scrutinee, isBound: Boolean): Tree = {
      assert(scrut.isDefined)
      val cond = handleOuter(condition(tpe, scrut.id, isBound))

      if (!needsOuterTest(tpe, scrut.tpe, owner)) cond
      else addOuterCondition(cond, tpe, scrut.id)
    }

    final def condition(tpe: Type, scrutTree: Tree, isBound: Boolean): Tree = {
      assert((tpe ne NoType) && (scrutTree.tpe ne NoType))
      def isMatchUnlessNull = scrutTree.tpe <:< tpe && tpe.isAnyRef
      def isRef             = scrutTree.tpe.isAnyRef

      // See ticket #1503 for the motivation behind checking for a binding.
      // The upshot is that it is unsound to assume equality means the right
      // type, but if the value doesn't appear on the right hand side of the
      // match that's unimportant; so we add an instance check only if there
      // is a binding.
      def bindingWarning() = {
        if (isBound && settings.Xmigration28.value) {
          cunit.warning(scrutTree.pos,
            "A bound pattern such as 'x @ Pattern' now matches fewer cases than the same pattern with no binding.")
        }
      }

      def genEquals(sym: Symbol): Tree = {
        val t1: Tree = REF(sym) MEMBER_== scrutTree

        if (isBound) {
          bindingWarning()
          t1 AND (scrutTree IS tpe.widen)
        }
        else t1
      }

      typer typed {
        tpe match {
          case ConstantType(Constant(null)) if isRef  => scrutTree OBJ_EQ NULL
          case ConstantType(const)                    => scrutTree MEMBER_== Literal(const)
          case SingleType(NoPrefix, sym)              => genEquals(sym)
          case SingleType(pre, sym) if sym.isStable   => genEquals(sym)
          case ThisType(sym) if sym.isModule          => genEquals(sym)
          case _ if isMatchUnlessNull                 => scrutTree OBJ_NE NULL
          case _                                      => scrutTree IS tpe
        }
      }
    }

    /** adds a test comparing the dynamic outer to the static outer */
    final def addOuterCondition(cond: Tree, tpe2test: Type, scrut: Tree) = {
      val TypeRef(prefix, _, _) = tpe2test
      val theRef = handleOuter(prefix match {
        case NoPrefix         => abort("assertion failed: NoPrefix")
        case ThisType(clazz)  => THIS(clazz)
        case pre              => REF(pre.prefix, pre.termSymbol)
      })
      outerAccessor(tpe2test.typeSymbol) match {
        case NoSymbol => ifDebug(cunit.warning(scrut.pos, "no outer acc for " + tpe2test.typeSymbol)) ; cond
        case outerAcc =>
          val casted = gen.mkAsInstanceOf(scrut, tpe2test, any = true, wrapInApply = true)
          cond AND ((casted DOT outerAcc)() OBJ_EQ theRef)
      }
    }
  }
}
