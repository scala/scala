/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */

package scala.tools.nsc
package matching

import util.Position
import transform.ExplicitOuter
import symtab.Flags
import collection._
import mutable.ListBuffer
import immutable.IntMap
import annotation.elidable

trait ParallelMatching extends ast.TreeDSL
      with MatchSupport
      with Matrix
      with Patterns
      with PatternBindings
{
  self: ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions.{ AnyRefClass, IntClass, BooleanClass, getProductArgs, productProj }
  import CODE._
  import Types._
  import Debug._
  import Flags.{ TRANS_FLAG }
  import PartialFunction._

  /** Transition **/
  def toPats(xs: List[Tree]): List[Pattern] = xs map Pattern.apply

  /** The umbrella matrix class. **/
  abstract class MatchMatrix(val context: MatrixContext) extends MatchMatrixOptimizer with MatrixExhaustiveness {
    import context._

    def data: MatrixContext#MatrixInit

    lazy val MatrixInit(roots, cases, failTree)  = data
    lazy val ExpandedMatrix(rows, targets)       = expand(roots, cases)
    lazy val expansion: Rep                      = make(roots, rows)

    val shortCuts   = new ListBuffer[Symbol]()

    final def shortCut(theLabel: Symbol): Int = {
      shortCuts += theLabel
      -shortCuts.length
    }

    /** first time bx is requested, a LabelDef is returned. next time, a jump.
     *  the function takes care of binding
     */
    final def requestBody(bx: Int, subst: Bindings): Tree = {
      // shortcut
      if (bx < 0)
        return Apply(ID(shortCuts(-bx-1)), Nil)

      val pvgroup = PatternVarGroup.fromBindings(subst.get(), targets(bx).freeVars)
      val target = targets(bx)

      // first time this bx is requested - might be bound elsewhere
      if (target.isNotReached) target.createLabelBody(bx, pvgroup)
      // call label "method" if possible
      else target.getLabelBody(pvgroup)
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(tvars: PatternVarGroup, row1: List[Row]): Rep = {
      // TRACE("make(%s%s)", pp(tvars.pvs, 1, true), pp(row1, 1, true))
      def classifyPat(opat: Pattern, j: Int): Pattern = opat simplify tvars(j)

      val rows = row1 flatMap (_ expandAlternatives classifyPat)
      if (rows.length != row1.length) make(tvars, rows)  // recursive call if any change
      else Rep(tvars, rows).checkExhaustive
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
      def isDefined   = sym ne NoSymbol
      def isSimple    = tpe.isByte || tpe.isShort || tpe.isChar || tpe.isInt
      def isCaseClass = tpe.typeSymbol.isCase

      // sequences
      def seqType         = tpe.widen baseType SeqClass
      def elemType        = tpe typeArgs 0

      private def elemAt(i: Int)  = (id DOT (tpe member nme.apply))(LIT(i))
      private def createElemVar(i: Int)   = createVar(elemType, _ => elemAt(i))
      private def createSeqVar(drop: Int) = createVar(seqType, _ => id DROP drop)

      def createSequenceVars(count: Int): List[PatternVar] =
        (0 to count).toList map (i => if (i < count) createElemVar(i) else createSeqVar(i))

      // for propagating "unchecked" to synthetic vars
      def isChecked = !(sym hasFlag TRANS_FLAG)
      def flags: List[Long] = List(TRANS_FLAG) filter (sym hasFlag _)

      // this is probably where this actually belongs
      def createVar(tpe: Type, f: Symbol => Tree) = context.createVar(tpe, f, isChecked)

      def castedTo(headType: Type) =
        if (tpe =:= headType) this
        else new Scrutinee(createVar(headType, lhs => id AS_ANY lhs.tpe))

      override def toString() = "(%s: %s)".format(id, tpe)
    }

    def isPatternSwitch(scrut: Scrutinee, ps: List[Pattern]): Option[PatternSwitch] = {
      def isSwitchableConst(x: Pattern) = cond(x) { case x: LiteralPattern if x.isSwitchable => true }
      def isSwitchableDefault(x: Pattern) = isSwitchableConst(x) || x.isDefault

      // TODO - scala> (5: Any) match { case 5 => 5 ; case 6 => 7 }
      // ... should compile to a switch.  It doesn't because the scrut isn't Int/Char, but
      // that could be handle in an if/else since every pattern requires an Int.
      // More immediately, Byte and Short scruts should also work.
      if (!scrut.isSimple) None
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
      require(scrut.isSimple && (ps forall (_.isSwitchable)))
    }

    case class PatternMatch(scrut: Scrutinee, ps: List[Pattern]) {
      def head = ps.head
      def tail = ps.tail
      def size = ps.length

      def headType = head.necessaryType
      def isCaseHead = head.isCaseClass
      private val dummyCount = if (isCaseHead) headType.typeSymbol.caseFieldAccessors.length else 0
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

      object TypedUnapply {
        def unapply(x: Tree): Option[Boolean] = condOpt(x) {
          case Typed(UnapplyParamType(tpe), tpt) => !(tpt.tpe <:< tpe)
        }
      }

      def mkRule(rest: Rep): RuleApplication = {
        tracing("Rule", head match {
          case x if isEquals(x.tree.tpe)        => new MixEquals(this, rest)
          case x: SequencePattern               => new MixSequence(this, rest, x)
          case AnyUnapply(false)                => new MixUnapply(this, rest, false)
          case _ =>
            isPatternSwitch(scrut, ps) match {
              case Some(x)  => new MixLiteralInts(x, rest)
              case _        => new MixTypes(this, rest)
            }
        })
      }
      override def toString() = "%s match {%s}".format(scrut, indentAll(ps))
    } // PatternMatch

    /** picks which rewrite rule to apply
     *  @precondition: column does not contain alternatives
     */
    def MixtureRule(scrut: Scrutinee, column: List[Pattern], rest: Rep): RuleApplication =
      PatternMatch(scrut, column) mkRule rest

    /**
     * Class encapsulating a guard expression in a pattern match:
     *   case ... if(tree) => ...
     */
    case class Guard(tree: Tree) {
      def isEmpty   = tree.isEmpty
      def duplicate = Guard(tree.duplicate)
      override def toString() = if (isEmpty) "" else " // if %s" format tree
    }
    val NoGuard = Guard(EmptyTree)

    /***** Rule Applications *****/

    sealed abstract class RuleApplication {
      // def isFinal = false
      // def body = tree
      // def freeVars = (scrut.pv :: rest.tvars).syms

      def pmatch: PatternMatch
      def rest: Rep
      def cond: Tree
      def success: Tree
      def failure: Tree

      lazy val PatternMatch(scrut, patterns) = pmatch
      lazy val head = pmatch.head
      def codegen: Tree = IF (cond) THEN (success) ELSE (failure)

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
    case class VariableRule(subst: Bindings, guard: Guard, guardedRest: Rep, bx: Int) extends RuleApplication {
      def pmatch: PatternMatch = impossible
      def rest: Rep = guardedRest

      lazy val cond     = if (guard.isEmpty) TRUE else guard.duplicate.tree
      lazy val success  = requestBody(bx, subst)
      lazy val failure  = guardedRest.toTree

      lazy val pvgroup  = PatternVarGroup.fromBindings(subst.get())

      final def tree(): Tree =
        if (guard.isEmpty) success
        else squeezedBlock(pvgroup.valDefs, codegen)
    }

    /** Mixture rule for all literal ints (and chars) i.e. hopefully a switch
     *  will be emitted on the JVM.
     */
    class MixLiteralInts(val pmatch: PatternSwitch, val rest: Rep) extends RuleApplication
    {
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
        case None                 => (Nil, Nil)
        case Some(Pattern(_, vs)) => (vs, List(rebindAll(rest rows literals.size, vs, scrut.sym)))
      }
      // literalMap is a map from each literal to a list of row indices.
      // varMap is a list from each literal to a list of the defined vars.
      lazy val (literalMap, varMap) = {
        val tags    = literals map (_.intValue)
        val varMap  = tags zip (literals map (_.deepBoundVariables))
        val litMap  =
          tags.zipWithIndex.reverse.foldLeft(IntMap.empty[List[Int]]) {
            // we reverse before the fold so the list can be built with ::
            case (map, (tag, index)) => map.updated(tag, index :: map.getOrElse(tag, Nil))
          }

        (litMap, varMap)
      }

      lazy val cases =
        for ((tag, indices) <- literalMap.toList) yield {
          val newRows = indices map (i => addDefaultVars(i)(rest rows i))
          val r   = remake(newRows ::: defaultRows, includeScrut = false)
          val r2  = make(r.tvars, r.rows map (x => x rebind bindVars(tag, x.subst)))

          CASE(Literal(tag)) ==> r2.toTree
        }

      lazy val defaultTree  = remake(defaultRows, includeScrut = false).toTree
      def casesWithDefault  = cases ::: List(CASE(WILD(IntClass.tpe)) ==> defaultTree)

      // cond/success/failure only used if there is exactly one case.
      lazy val (cond, success) = cases match {
        case List(CaseDef(lit, _, body))  => (scrut.id MEMBER_== lit, body)
      }
      lazy val failure = defaultTree

      // only one case becomes if/else, otherwise match
      def tree() =
        if (cases.size == 1) codegen
        else casted MATCH (casesWithDefault: _*)
    }

    /** mixture rule for unapply pattern
     */
    class MixUnapply(val pmatch: PatternMatch, val rest: Rep, typeTest: Boolean) extends RuleApplication {
      val uapattern = head match { case x: UnapplyPattern => x ; case _ => abort("XXX") }
      val ua @ UnApply(app, args) = head.tree

      // Note: trailingArgs is not necessarily Nil, because unapply can take implicit parameters.
      val Apply(fxn, _ :: trailingArgs) = app
      private def reapply = Apply(fxn, scrut.id :: trailingArgs)

      private lazy val zipped      = pmatch pzip rest.rows

      lazy val unapplyResult: PatternVar =
        scrut.createVar(app.tpe, lhs => reapply setType lhs.tpe)

      // XXX in transition.
      object sameUnapplyCall {
        private def sameFunction(fn1: Tree) = fxn.symbol == fn1.symbol && (fxn equalsStructure fn1)

        def unapply(p: Pattern) = condOpt(p) {
          case Pattern(UnApply(Apply(fn1, _), args), _) if sameFunction(fn1)  =>
            tracing("sameUnapply", args)
        }
      }
      object SameUnapply {
        def unapply(p: Pattern) = p match {
          case x: UnapplyPattern if uapattern isSameUnapply x => Some(args)
          case _                                              => None
        }
      }
      def isSameUnapply(p: Pattern) = SameUnapply.unapply(p).isDefined

      lazy val cond: Tree =
        if (unapplyResult.tpe.isBoolean) ID(unapplyResult.valsym)
        else unapplyResult.valsym IS_DEFINED

      lazy val failure =
        mkFail(zipped.tail filterNot (x => isSameUnapply(x._1)) map { case (pat, r) => r insert pat })

      private def doSuccess: (List[PatternVar], List[PatternVar], List[Row]) = {
        // pattern variable for the unapply result of Some(x).get
        lazy val pv = scrut.createVar(
          app.tpe typeArgs 0,
          _ => fn(ID(unapplyResult.lhs), nme.get)
        )
        def tuple = pv.lhs

        // at this point it's Some[T1,T2...]
        lazy val tpes  = getProductArgs(tuple.tpe).get

        // one pattern variable per tuple element
        lazy val tuplePVs =
          for ((tpe, i) <- tpes.zipWithIndex) yield
            scrut.createVar(tpe, _ => fn(ID(tuple), productProj(tuple, i + 1)))

        // the filter prevents infinite unapply recursion
        def mkNewRows(sameFilter: (List[Tree]) => List[Tree]) = {
          val dum = if (args.length <= 1) args.length else tpes.size
          for ((pat, r) <- zipped) yield pat match {
            case sameUnapplyCall(xs)  => r.insert2(toPats(sameFilter(xs)) ::: List(NoPattern), pat.boundVariables, scrut.sym)
            case _                    => r insert (emptyPatterns(dum) ::: List(pat))
          }
        }

        // 0 is Boolean, 1 is Option[T], 2+ is Option[(T1,T2,...)]
        args.length match {
          case 0  => (Nil, Nil, mkNewRows((xs) => Nil))
          case 1  => (List(pv), List(pv), mkNewRows(xs => List(xs.head)))
          case _  => (pv :: tuplePVs, tuplePVs, mkNewRows(identity))
        }
      }

      lazy val success = {
        val (squeezePVs, pvs, rows) = doSuccess
        val srep = remake(rows, pvs).toTree

        squeezedBlockPVs(squeezePVs, srep)
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

      // one pattern var per sequence element up to elemCount, and one more for the rest of the sequence
      lazy val pvs = scrut createSequenceVars pivotLen

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
        nullSafe(compareFn, FALSE)(scrut.id)
      }
      lazy val success  = squeezedBlockPVs(pvs, remake(successRows, pvs, hasStar).toTree)
      lazy val failure  = remake(failRows).toTree

      final def tree(): Tree = codegen
    }

    // @todo: equals test for same constant
    class MixEquals(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      private lazy val labelBody =
        remake((rest.rows.tail, pmatch.tail).zipped map (_ insert _)).toTree

      private lazy val rhs =
        decodedEqualsType(head.tpe) match {
          case SingleType(pre, sym) => REF(pre, sym)
          case PseudoType(o)        => o.duplicate
        }

      lazy val label =
        owner.newLabel(scrut.pos, newName("failCont%")) setInfo MethodType(Nil, labelBody.tpe)

      lazy val cond =
        handleOuter(rhs MEMBER_== scrut.id )

      lazy val success = remake(List(
        rest.rows.head.insert2(List(NoPattern), head.boundVariables, scrut.sym),
        Row(emptyPatterns(1 + rest.tvars.size), NoBinding, NoGuard, shortCut(label))
      )).toTree

      lazy val failure = LabelDef(label, Nil, labelBody)

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
            case Pattern(LIT(null), _) if !(p =:= s)        => (None, passr)      // (1)
            case x if isObjectTest                          => (passl(), None)    // (2)
            case Pattern(Typed(pp, _), _)     if sMatchesP  => (typed(pp), None)  // (4)
            // The next line used to be this which "fixed" 1697 but introduced
            // numerous regressions including #3136.
            // case Pattern(_: UnApply, _)                     => (passl(), passr)
            case Pattern(_: UnApply, _)                     => (None, passr)
            case x if !x.isDefault && sMatchesP             => (subs(), None)
            case x if  x.isDefault || pMatchesS             => (passl(), passr)
            case _                                          => (None, passr)
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

    case class Row(pats: List[Pattern], subst: Bindings, guard: Guard, bx: Int) {
      private def nobindings = subst.get().isEmpty
      private def bindstr = if (nobindings) "" else pp(subst)
      if (pats exists (p => !p.isDefault))
        traceCategory("Row", "%s%s", pats, bindstr)

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
        tracing("insert2", copy(pats = hs ::: pats, subst = subst.add(vs, tvar)))

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

    object ExpandedMatrix {
      def unapply(x: ExpandedMatrix) = Some((x.rows, x.targets))
      def apply(rowz: List[(Row, FinalState)]) =
        new ExpandedMatrix(rowz map (_._1), rowz map (_._2))
    }

    class ExpandedMatrix(val rows: List[Row], val targets: List[FinalState]) {
      require(rows.size == targets.size)

      override def toString() = {
        def vprint(vs: List[Any]) = if (vs.isEmpty) "" else ": %s".format(pp(vs))
        def rprint(r: Row) = pp(r)
        def tprint(t: FinalState) =
          if (t.freeVars.isEmpty) " ==> %s".format(pp(t.body))
          else " ==>\n        %s".format(pp(t.freeVars -> t.body))

        val xs = rows zip targets map { case (r,t) => rprint(r) + tprint(t) }
        val ppstr = pp(xs, newlines = true)

        "ExpandedMatrix(%d rows)".format(rows.size) + ppstr
      }
    }

    abstract class State {
      def body: Tree
      def freeVars: List[Symbol]
      def isFinal: Boolean
    }

    case class FinalState(bx: Int, body: Tree, freeVars: List[Symbol]) extends State {
      private var referenceCount = 0
      private var _label: LabelDef = null
      private var _labelSym: Symbol = null

      def labelSym = _labelSym
      def label = _label

      // @bug: typer is not able to digest a body of type Nothing being assigned result type Unit
      def bodyTpe = if (body.tpe.isNothing) body.tpe else matchResultType
      def duplicate = body.duplicate setType bodyTpe

      def isFinal = true
      def isLabellable = !cond(body)  { case _: Literal => true }
      def isNotReached = referenceCount == 0
      def isReachedOnce = referenceCount == 1
      def isReachedTwice = referenceCount > 1

      // arguments to pass to this body%xx
      def labelParamTypes = label.tpe.paramTypes

      def createLabelBody(index: Int, pvgroup: PatternVarGroup) = {
        val args = pvgroup.syms
        val vdefs = pvgroup.valDefs

        val name = "body%" + index
        require(_labelSym == null)
        referenceCount += 1

        if (isLabellable) {
          val mtype = MethodType(freeVars, bodyTpe)
          _labelSym = owner.newLabel(body.pos, name) setInfo mtype

          TRACE("Creating index %d: mtype = %s".format(bx, mtype))
          _label = typer typedLabelDef LabelDef(_labelSym, freeVars, body setType bodyTpe)
          TRACE("[New label] def %s%s: %s = %s".format(name, pp(freeVars), bodyTpe, body))
        }

        ifLabellable(vdefs, squeezedBlock(vdefs, label))
      }

      def getLabelBody(pvgroup: PatternVarGroup): Tree = {
        val idents = pvgroup map (_.rhs)
        val vdefs = pvgroup.valDefs
        referenceCount += 1

        ifLabellable(vdefs, ID(labelSym) APPLY (idents))
      }

      private def ifLabellable(vdefs: List[Tree], t: => Tree) =
        if (isLabellable) t
        else squeezedBlock(vdefs, duplicate)

      override def toString() = pp("Final%d%s".format(bx, pp(freeVars)) -> body)
    }

    case class Rep(val tvars: PatternVarGroup, val rows: List[Row]) {
      lazy val Row(pats, subst, guard, index) = rows.head
      lazy val guardedRest        = if (guard.isEmpty) NoRep else make(tvars, rows.tail)
      lazy val (defaults, others) = pats span (_.isDefault)

      /** Sealed classes. */
      def checkExhaustive = new ExhaustivenessChecker(this).check

      /** Cut out the column containing the non-default pattern. */
      class Cut(index: Int) {
        /** The first two separate out the 'i'th pattern in each row from the remainder. */
        private val (_column, _rows) = rows map (_ extractColumn index) unzip

        /** Now the 'i'th tvar is separated out and used as a new Scrutinee. */
        private val (_pv, _tvars) = tvars extractIndex index

        /** The non-default pattern (others.head) replaces the column head. */
        private val (_ncol, _nrep) =
          (others.head :: _column.tail, make(_tvars, _rows))

        def mix = MixtureRule(new Scrutinee(specialVar(_pv.sym, _pv.checked)), _ncol, _nrep)
      }

      /** Converts this to a tree - recursively acquires subreps. */
      final def toTree(): Tree = tracing("toTree", typer typed applyRule())

      /** The VariableRule. */
      private def variable() = {
        val binding = (defaults map (_.boundVariables) zip tvars.pvs) .
          foldLeft(subst)((b, pair) => b.add(pair._1, pair._2.lhs))

        VariableRule(binding, guard, guardedRest, index)
      }

      /** The MixtureRule. */
      def mixture() = new Cut(defaults.size) mix

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
        if (tvars.size == 0) "Rep(%d) = %s".format(rows.size, ppn(rows))
        else "Rep(%dx%d)%s%s".format(tvars.size, rows.size, ppn(tvars), ppn(rows))
    }

    val NoRep = Rep(Nil, Nil)
    /** Expands the patterns recursively. */
    final def expand(roots: List[PatternVar], cases: List[CaseDef]) =
      tracing("Expanded", ExpandedMatrix(
        for ((CaseDef(pat, guard, body), index) <- cases.zipWithIndex) yield {
          def mkRow(ps: List[Tree]) = Row(toPats(ps), NoBinding, Guard(guard), index)

          val pattern = Pattern(pat)
          val row = mkRow(pat match {
            case x if roots.length <= 1 => List(x)
            case Apply(_, args)         => args
            case WILD()                 => emptyTrees(roots.length)
          })

          row -> FinalState(index, body, pattern.deepBoundVariables)
        })
      )

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
          case ConstantType(Constant(value))          => scrutTree MEMBER_== Literal(value)
          case SingleType(NoPrefix, sym)              => genEquals(sym)
          case SingleType(pre, sym) if sym.isModule   => genEquals(sym)
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
        case outerAcc => cond AND (((scrut AS_ANY tpe2test) DOT outerAcc)() OBJ_EQ theRef)
      }
    }
  }
}
