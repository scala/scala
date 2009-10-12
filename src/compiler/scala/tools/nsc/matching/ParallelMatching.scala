/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc
package matching

import util.Position
import transform.ExplicitOuter
import symtab.Flags
import collection._
import mutable.ListBuffer
import immutable.IntMap
import annotation.elidable
import Function.tupled

trait ParallelMatching extends ast.TreeDSL
      with MatchSupport
      with Matrix
      with Patterns
      with PatternBindings
{
  self: ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions.{ AnyRefClass, IntClass, BooleanClass, getProductArgs, productProj }
  import treeInfo.{ isStar }
  import CODE._
  import Types._
  import Debug._
  import Flags.{ TRANS_FLAG }

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

    // XXX transitional.
    final def requestBody(bx: Int, subst: Bindings): Tree =
      requestBody(bx, PatternVarGroup.fromBindings(subst.get(), targets(bx).freeVars))

    /** first time bx is requested, a LabelDef is returned. next time, a jump.
     *  the function takes care of binding
     */
    final def requestBody(bx: Int, pvgroup: PatternVarGroup): Tree = {
      val target = targets(bx)

      // shortcut
      if (bx < 0) Apply(ID(shortCuts(-bx-1)), Nil)
      // first time this bx is requested - might be bound elsewhere
      else if (target.isNotReached) target.createLabelBody(bx, pvgroup)
      // call label "method" if possible
      else target.getLabelBody(pvgroup)
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(tvars: PatternVarGroup, row1: List[Row]): Rep = {
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
      def id    = ID(sym)   // attributed ident

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
      def isCaseClass = tpe.typeSymbol hasFlag Flags.CASE

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
        val lits = _lits filterMap { case x: LiteralPattern => x }

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
      // def dummies = head.dummies

      def apply(i: Int): Pattern = ps(i)
      def pzip() = ps.zipWithIndex
      def pzip[T](others: List[T]) = {
        assert(ps.size == others.size, "Internal error: ps = %s, others = %s".format(ps, others))
        ps zip others
      }

      // Any unapply - returns Some(true) if a type test is needed before the unapply can
      // be called (e.g. def unapply(x: Foo) = { ... } but our scrutinee is type Any.)
      object AnyUnapply {
        def unapply(x: Tree): Option[Boolean] = condOpt(x) { case UnapplyParamType(tpe) => !(scrut.tpe <:< tpe) }
      }

      def mkRule(rest: Rep): RuleApplication = {
        tracing("Rule", head.tree match {
            case x if isEquals(x.tpe)                 => new MixEquals(this, rest)
            case x: ArrayValue                        => new MixSequence(this, rest)
            case AnyUnapply(false)                    => new MixUnapply(this, rest, false)
            case _ =>
              isPatternSwitch(scrut, ps) match {
                case Some(x)  => new MixLiteralInts(x, rest)
                case _        => new MixTypes(this, rest)
              }
          }
        )
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

      final def tree(): Tree = squeezedBlock(pvgroup.valDefs, codegen)
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

    /** handle sequence pattern and ArrayValue (but not star patterns)
     */
    sealed class MixSequence(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      // Called 'pivot' since it's the head of the column under consideration in the mixture rule.
      val pivot @ SequencePattern(av @ ArrayValue(_, _)) = head
      private def pivotLen = pivot.nonStarLength

      def mustCheck(first: Pattern, next: Pattern): Boolean = {
        if (first.tree eq next.tree)
          return false

        !(first completelyCovers next)
      }

      def getSubPatterns(x: Pattern): Option[List[Pattern]] = {
        // def defaults = Some(emptyPatterns(pivot.elemPatterns.length + 1))
        def defaults = Some(pivot.dummies)
        val av @ ArrayValue(_, xs) = x.tree match {
          case x: ArrayValue      => x
          case EmptyTree | WILD() => return defaults
          case _                  => return None
        }

        val sp = x.asInstanceOf[SequencePattern]
        val (star1, star2) = (pivot.hasStar, sp.hasStar)

        if (sp.nonStarLength == pivotLen) {
          Some((star1, star2) match {
            case (true, true)   => (sp rebindStar scrut.seqType) ::: List(NoPattern)
            case (true, false)  => toPats(xs ::: List(gen.mkNil, EmptyTree))
            case (false, true)  => (sp rebindStar scrut.seqType)
            case (false, false) => toPats(xs) ::: List(NoPattern)
          })
        }
        else if (pivot.hasStar && sp.hasStar && xs.length-1 < pivotLen)
          Some(emptyPatterns(pivotLen + 1) ::: List(x))
        else
          defaults    // XXX
      }

      lazy val cond = (pivot precondition pmatch).get

      lazy val (success, failure) = {
        assert(scrut.tpe <:< head.tpe, "fatal: %s is not <:< %s".format(scrut, head.tpe))

        // one pattern var per sequence element up to elemCount, and one more for the rest of the sequence
        val pvs = scrut createSequenceVars pivot.nonStarPatterns.size

        val (nrows, frows): (List[Option[Row]], List[Option[Row]]) = List.unzip(
          for ((c, rows) <- pmatch pzip rest.rows) yield getSubPatterns(c) match {
            case Some(ps) => (Some(rows insert ps), if (mustCheck(head, c)) Some(rows insert c) else None)
            case None     => (None, Some(rows insert c))
          }
        )

        val succ = remake(nrows.flatten, pvs, includeScrut = pivot.hasStar)

        (
          squeezedBlockPVs(pvs, succ.toTree),
          remake(frows.flatten).toTree
        )
      }

      final def tree(): Tree = codegen
    }

    // @todo: equals test for same constant
    class MixEquals(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      private lazy val labelBody =
        remake(List.map2(rest.rows.tail, pmatch.tail)(_ insert _)).toTree

      private lazy val rhs =
        decodedEqualsType(head.tpe) match {
          case SingleType(pre, sym) => REF(pre, sym)
          case PseudoType(o)        => o.duplicate
        }

      lazy val label =
        owner.newLabel(scrut.pos, newName(scrut.pos, "failCont%")) setInfo MethodType(Nil, labelBody.tpe)

      lazy val cond =
        handleOuter(scrut.id MEMBER_== rhs)

      lazy val success = remake(List(
        rest.rows.head.insert2(List(NoPattern), head.boundVariables, scrut.sym),
        Row(emptyPatterns(1 + rest.tvars.size), NoBinding, NoGuard, shortCut(label))
      )).toTree

      lazy val failure = LabelDef(label, Nil, labelBody)

      final def tree() = codegen
      override def toString() = "MixEquals(%s == %s)".format(scrut, head)
    }

    /** mixture rule for type tests
    **/
    class MixTypes(val pmatch: PatternMatch, val rest: Rep) extends RuleApplication {
      // see bug1434.scala for an illustration of why "x <:< y" is insufficient.
      // this code is definitely inadequate at best.  Inherited comment:
      //
      //   an approximation of _tp1 <:< tp2 that ignores _ types. this code is wrong,
      //   ideally there is a better way to do it, and ideally defined in Types.scala
      private def matches(arg1: Type, arg2: Type) = {
        val List(t1, t2) = List(arg1, arg2) map decodedEqualsType
        def eqSymbols = t1.typeSymbol eq t2.typeSymbol
        //  note: writing this as "t1.baseTypeSeq exists (_ =:= t2)" does not lead to 1434 passing.
        def isSubtype = t1.baseTypeSeq exists (_.typeSymbol eq t2.typeSymbol)

        (t1 <:< t2) || ((t1, t2) match {
          case (_: TypeRef, _: TypeRef) => !t1.isArray && (t1.prefix =:= t2.prefix) && (eqSymbols || isSubtype)
          case _ => false
        })
      }

      // moreSpecific: more specific patterns
      //     subsumed: more general patterns (subsuming current), rows index and subpatterns
      //    remaining: remaining, rows index and pattern
      def join[T](xs: List[Option[T]]): List[T] = xs.flatMap(x => x)
      val (moreSpecific, subsumed, remaining) : (List[Pattern], List[(Int, List[Pattern])], List[(Int, Pattern)]) = unzip3(
        for ((pattern, j) <- pmatch.pzip()) yield {
          // scrutinee, head of pattern group
          val (s, p) = (pattern.tpe, pmatch.headType)

          def sMatchesP = matches(s, p)
          def pMatchesS = matches(p, s)

          def alts[T](yes: T, no: T): T = if (p =:= s) yes else no
          def isObjectTest              = pattern.isObject && (p =:= pattern.necessaryType)

          lazy val dummy          = (j, pmatch.dummies)
          lazy val pass           = (j, pattern)
          lazy val subs           = (j, pattern subpatterns pmatch)

          // each pattern will yield a triple of options corresponding to the three lists,
          // which will be flattened down to the values
          implicit def mkOpt[T](x: T): Option[T] = Some(x)    // limits noise from Some(value)

          // Note - at the moment these comments are mostly trivial or nonsensical, but
          // they persist from a much earlier time and I still try to read the tea leaves
          //
          // (1) special case for constant null
          // (2) matching an object
          // (3) <:< is never <equals>
          // (4) never =:= for <equals>

          (pattern match {
            case Pattern(LIT(null), _) if !(p =:= s)                            => (None, None, pass)                         // (1)
            case x if isObjectTest                                              => (NoPattern, dummy, None)                   // (2)
            // case Pattern(Typed(pp @ Pattern(_: UnApply, _), _), _) if sMatchesP => (Pattern(pp), dummy, None)                 // (3)
            case Pattern(Typed(pp, _), _) if sMatchesP                          => (alts(Pattern(pp), pattern), dummy, None)  // (4)
            case Pattern(_: UnApply, _)                                         => (NoPattern, dummy, pass)
            case x if !x.isDefault && sMatchesP                                 => (alts(NoPattern, pattern), subs, None)
            case x if  x.isDefault || pMatchesS                                 => (NoPattern, dummy, pass)
            case _                                                              => (None, None, pass)

            // The below (back when the surrounding code looked much different) fixed bugs 425 and 816
            // with only the small downside of causing 60 other tests to fail.
            // case _ =>
            //   if (erased_xIsaY || xIsaY && !isDef)  (alts(EmptyTree, pat), subs, None) // never =:= for <equals>
            //   else if (isDef)                       (EmptyTree,           dummy, pass)
            //   else                                  (None,                 None, pass)

          }) : (Option[Pattern], Option[(Int, List[Pattern])], Option[(Int, Pattern)])
        }
      ) match { case (x,y,z) => (join(x), join(y), join(z)) }

      // temporary checks so we're less crashy while we determine what to implement.
      def checkErroneous(scrut: Scrutinee): Type = {
        scrut.tpe match {
          case tpe @ ThisType(_) if tpe.termSymbol == NoSymbol        =>
            cunit.error(scrut.pos, "self type test in anonymous class forbidden by implementation.")
            ErrorType
          case x => x
        }
      }

      override def toString = {
        val msgs = List(
          "moreSpecific: " + pp(moreSpecific),
          "    subsumed: " + pp(subsumed),
          "   remaining: " + pp(remaining)
        )

        super.toString() + "\n" + indentAll(msgs)
      }

      private def isAnyMoreSpecific = moreSpecific exists (x => !x.isEmpty)
      private def mkZipped          = moreSpecific zip subsumed map {
        case (mspat, (j, pmatch)) => (j, mspat :: pmatch)
      }

      lazy val casted = scrut castedTo pmatch.headType
      lazy val cond   = condition(checkErroneous(casted), scrut)

      lazy val success = {
        val (subtests, subtestVars) =
          if (isAnyMoreSpecific)  (mkZipped, List(casted.pv))
          else                    (subsumed, Nil)

        val newRows =
          for ((j, ps) <- subtests) yield
            (rest rows j).insert2(ps, pmatch(j).boundVariables, casted.sym)

        val srep =
          remake(newRows, subtestVars ::: casted.accessorPatternVars, includeScrut = false)

        squeezedBlock(casted.allValDefs, srep.toTree)
      }
      lazy val failure =
        mkFail(remaining map tupled((p1, p2) => rest rows p1 insert p2))

      final def tree(): Tree = codegen
    }

    /*** States, Rows, Etc. ***/

    case class Row(pats: List[Pattern], subst: Bindings, guard: Guard, bx: Int) {
      private def substpp = if (subst.get().isEmpty) "" else "(free = %s)".format(pp(subst.get()))
      if (pats exists (p => !p.isDefault))
        traceCategory("Row", "%s%s", pats, substpp)

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
        val newPats: List[Pattern] = pats.zipWithIndex map tupled(classifyPat)
        // see if any alternatives were in there
        val (ps, others) = newPats span isNotAlternative
        // make a new row for each alternative, with it spliced into the original position
        if (others.isEmpty) List(copy(pats = ps))
        else extractBindings(others.head) map (x => replaceAt(ps.size, x))
      }
      override def toString() = pp(bx -> (pats, subst))
    }

    object ExpandedMatrix {
      def unapply(x: ExpandedMatrix) = Some((x.rows, x.targets))
      def apply(rowz: List[(Row, FinalState)]) =
        new ExpandedMatrix(rowz map (_._1), rowz map (_._2))
    }

    class ExpandedMatrix(val rows: List[Row], val targets: List[FinalState]) {
      require(rows.size == targets.size)

      override def toString() = {
        def rprint(r: Row) = "Row %d: %d pats, %d bound".format(r.bx, r.pats.size, r.subst.get().size)
        def tprint(t: FinalState) = "%s (free = %s)".format(t.body, pp(t.freeVars))
        val xs = rows zip targets map { case (r,t) => rprint(r) -> tprint(t) }
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
      def isLabellable = !cond(body)  { case _: Throw | _: Literal => true }
      def isNotReached = referenceCount == 0
      def isReachedOnce = referenceCount == 1
      def isReachedTwice = referenceCount > 1

      // arguments to pass to this body%xx
      def labelParamTypes = label.tpe.paramTypes

      private def consistencyFailure(idents: List[Tree], vdefs: List[Tree]) = {
        val LabelDef(name, params, rhs) = label

        val msg = "Consistency failure in generated block %s(%s):\n  idents = %s\n  vdefs = %s\n"
        abort(msg.format(name, pp(labelParamTypes), pp(idents), pp(vdefs)))
      }

      def createLabelBody(index: Int, pvgroup: PatternVarGroup) = {
        def args = pvgroup.syms
        def vdefs = pvgroup.valDefs

        val name = "body%" + index
        require(_labelSym == null)
        referenceCount += 1

        if (isLabellable) {
          // val mtype = MethodType(freeVars, bodyTpe)
          val mtype = MethodType(args, bodyTpe)
          _labelSym = owner.newLabel(body.pos, name) setInfo mtype

          TRACE("Creating index %d: mtype = %s".format(bx, mtype))
          if (freeVars.size != args.size)
            TRACE("We will be hosed! freeVars = %s, args = %s, vdefs = %s".format(freeVars, args, vdefs))

          // Labelled expression - the symbols in the array (must be Idents!)
          // are those the label takes as argument
          _label = typer typedLabelDef LabelDef(_labelSym, args, body setType bodyTpe)
          TRACE("[New label] def %s%s: %s = %s".format(name, pp(args), bodyTpe, body))
        }

        ifLabellable(vdefs, squeezedBlock(vdefs, label))
      }

      def getLabelBody(pvgroup: PatternVarGroup): Tree = {
        def idents = pvgroup map (_.rhs)
        def vdefs = pvgroup.valDefs
        referenceCount += 1
        // if (idents.size != labelParamTypes.size)
        //   consistencyFailure(idents, vdefs)

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
        private val (_column, _rows) =
          List.unzip(rows map (_ extractColumn index))

        /** Now the 'i'th tvar is separated out and used as a new Scrutinee. */
        private val (_pv, _tvars) =
          tvars extractIndex index

        /** The non-default pattern (others.head) replaces the column head. */
        private val (_ncol, _nrep) =
          (others.head :: _column.tail, make(_tvars, _rows))

        def mix = MixtureRule(new Scrutinee(specialVar(_pv.sym, _pv.checked)), _ncol, _nrep)
      }

      /** Converts this to a tree - recursively acquires subreps. */
      final def toTree(): Tree = typer typed this.applyRule()

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

      override def toString() =
        if (tvars.size == 0) "Rep(%d) = %s".format(rows.size, pp(rows))
        else "Rep(%dx%d)\n  %s\n  %s".format(tvars.size, rows.size, pp(tvars), pp(rows))
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
    final def condition(tpe: Type, scrut: Scrutinee): Tree = {
      assert(scrut.isDefined)
      val cond = handleOuter(condition(tpe, scrut.id))

      if (!needsOuterTest(tpe, scrut.tpe, owner)) cond
      else addOuterCondition(cond, tpe, scrut.id)
    }

    final def condition(tpe: Type, scrutTree: Tree): Tree = {
      assert((tpe ne NoType) && (scrutTree.tpe ne NoType))
      def useEqTest         = tpe.termSymbol.isModule || (tpe.prefix eq NoPrefix)

      //         case SingleType(_, _) | ThisType(_) | SuperType(_, _) =>
      //           val cmpOp = if (targ.tpe <:< AnyValClass.tpe) Any_equals else Object_eq
      // Apply(Select(qual, cmpOp), List(gen.mkAttributedQualifier(targ.tpe)))

      typer typed (tpe match {
        case ct: ConstantType => ct.value match {
            case v @ Constant(null) if scrutTree.tpe.isAnyRef   => scrutTree ANY_EQ NULL
            case v                                              => scrutTree MEMBER_== Literal(v)
          }
        case _: SingletonType if useEqTest                      => REF(tpe.termSymbol) MEMBER_== scrutTree
        case _ if scrutTree.tpe <:< tpe && tpe.isAnyRef         => scrutTree OBJ_!= NULL
        case _                                                  => scrutTree IS tpe
      })
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
        case outerAcc => cond AND (((scrut AS_ANY tpe2test) DOT outerAcc)() ANY_EQ theRef)
      }
    }
  }
}
