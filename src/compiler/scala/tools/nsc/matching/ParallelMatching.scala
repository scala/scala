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
import mutable.{ BitSet, HashMap, ListBuffer }
import immutable.IntMap
import MatchUtil._
import annotation.elidable
import Function.tupled

trait ParallelMatching extends ast.TreeDSL
      with Matrix
      with Patterns
      with PatternBindings
      with PatternOptimizer
{
  self: ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions.{ AnyRefClass, IntClass, getProductArgs, productProj }
  import treeInfo.{ isStar }
  import CODE._

  /** Debugging support: enable with -Ypmat-debug **/
  private final def trace = settings.Ypmatdebug.value

  object Types {
    import definitions._
    implicit def enrichType(x: Type): RichType = new RichType(x)

    class RichType(undecodedTpe: Type) {
      def tpe = decodedEqualsType(undecodedTpe)
      def isAnyRef = tpe <:< AnyRefClass.tpe

      // These tests for final classes can inspect the typeSymbol
      private def is(s: Symbol) = tpe.typeSymbol eq s
      def      isByte = is(ByteClass)
      def     isShort = is(ShortClass)
      def       isInt = is(IntClass)
      def      isChar = is(CharClass)
      def   isBoolean = is(BooleanClass)
      def   isNothing = is(NothingClass)
      def     isArray = is(ArrayClass)
    }
  }
  import Types._

  object Debug {
    def typeToString(t: Type): String = t match {
      case NoType => "x"
      case x      => x.toString
    }
    def symbolToString(s: Symbol): String = s match {
      case x  => x.toString
    }
    def treeToString(t: Tree): String = unbind(t) match {
      case EmptyTree            => "?"
      case WILD()               => "_"
      case Literal(Constant(x)) => "LIT(%s)".format(x)
      case Apply(fn, args)      => "%s(%s)".format(treeToString(fn), args map treeToString mkString ",")
      case x: TypeTree          => "TT(%s)".format(symbolToString(x.symbol))
      case Typed(expr, tpt)     => "%s: %s".format(treeToString(expr), treeToString(tpt))
      case x                    =>  x.toString + " (" + x.getClass + ")"
    }

    // pretty print for debugging
    def pp(x: Any): String = pp(x, false)
    def pp(x: Any, newlines: Boolean): String = {
      def clean(s: String): String = s.replaceAll("""java\.lang\.""", "")

      val elems: List[Any] = x match {
        case x: String      => return clean(x)
        case xs: List[_]    => xs
        case x: Tuple2[_,_] => return pp(x._1) + " -> " + pp(x._2)
        case x              => return pp(x.toString)
      }
      def pplist(xs: List[Any]): String = {
        val xs2 = xs map pp

        if (newlines) (xs2 map ("    " + _ + "\n")).mkString("\n", "", "")
        else xs2.mkString("(", ", ", ")")
      }

      pplist(elems)
    }

    def ifDebug(body: => Unit): Unit          = { if (settings.debug.value) body }
    def DBG(msg: => String): Unit             = { ifDebug(println(msg)) }

    // @elidable(elidable.FINE)
    def TRACE(f: String, xs: Any*): Unit      = { if (trace) println(pp(if (xs.isEmpty) f else f.format(xs : _*))) }

    def tracing2[T](x: T)(category: String, xs: String*) = {
      val preamble = "[" + """%10s""".format(category) + "]  "
      if (xs.isEmpty) TRACE(preamble, x)
      else TRACE(preamble + xs.head, xs.tail: _*)

      x
    }

    def tracing[T](s: String, x: T): T = {
      TRACE("[" + """%10s""".format(s) + "]  " + x.toString)
      x
    }
    def traceCategory(cat: String, f: String, xs: Any*) =
      TRACE("[" + """%10s""".format(cat) + "]  " + f, xs: _*)

    def indent(s: Any) = s.toString() split "\n" map ("  " + _) mkString "\n"
    def indentAll(s: Seq[Any]) = s map ("  " + _.toString() + "\n") mkString
  }
  import Debug._

  /** Transition **/
  def isRightIgnoring(t: Tree) = cond(unbind(t)) { case ArrayValue(_, xs) if !xs.isEmpty => isStar(xs.last) }
  def toPats(xs: List[Tree]): List[Pattern] = xs map Pattern.apply

  /** The umbrella matrix class. **/
  class MatchMatrix(val context: MatrixContext, data: MatrixInit) extends MatchMatrixOptimizer {
    import context._

    val MatrixInit(roots, cases, failTree)  = data
    val ExpandedMatrix(rows, targets)       = expand(roots, cases)
    val expansion: Rep                      = make(roots, rows)

    val shortCuts   = new ListBuffer[Symbol]()

    final def shortCut(theLabel: Symbol): Int = {
      shortCuts += theLabel
      -shortCuts.length
    }

    /** first time bx is requested, a LabelDef is returned. next time, a jump.
     *  the function takes care of binding
     */
    final def requestBody(bx: Int, subst: Bindings): Tree = {
      implicit val ctx = context
      lazy val target @ FinalState(tbx, body, freeVars) = targets(bx)
      lazy val substInfo = subst infoFor freeVars
      import substInfo._

      // XXX when is this not true?
      // assert(tbx == bx)

      // shortcut
      if (bx < 0) Apply(ID(shortCuts(-bx-1)), Nil)
      // first time this bx is requested - might be bound elsewhere
      else if (target.isNotReached) target.createLabelBody("body%"+bx, patternVars, patternValDefs)
      // call label "method" if possible
      else target.getLabelBody(idents, patternValDefs)
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(tvars: List[Symbol], row1: List[Row]): Rep = {
      def classifyPat(opat: Pattern, j: Int): Pattern = opat simplify tvars(j)

      val rows = row1 flatMap (_ expandAlternatives classifyPat)
      if (rows.length != row1.length) make(tvars, rows)  // recursive call if any change
      else Rep(tvars, rows).checkExhaustive
    }

    override def toString() = "MatchMatrix(%s) { %s }".format(matchResultType, indentAll(targets))

    /**
     * Encapsulates a symbol being matched on.
     *
     * sym match { ... }
     *
     * results in Scrutinee(sym).
     *
     * Note that we only ever match on Symbols, not Trees: a temporary variable
     * is created for any expressions being matched on.
     */
    case class Scrutinee(val sym: Symbol) {
      import definitions._

      // presenting a face of our symbol
      def tpe       = sym.tpe
      def pos       = sym.pos
      def id        = ID(sym)   // attributed ident

      def accessors     = if (isCaseClass) sym.caseFieldAccessors else Nil
      def accessorVars  = accessors map (a => newVarOfTpe((tpe memberType a).resultType))

      // tests
      def isDefined   = sym ne NoSymbol
      def isSimple    = tpe.isByte || tpe.isShort || tpe.isChar || tpe.isInt
      def isCaseClass = tpe.typeSymbol hasFlag Flags.CASE

      // sequences
      def seqType   = tpe.widen baseType SeqClass
      def elemType  = tpe typeArgs 0

      def newVarOfTpe(tpe: Type) = newVar(pos, tpe, flags)
      def newVarOfSeqType = newVar(pos, seqType)
      def newVarOfElemType = newVar(pos, elemType)

      // for propagating "unchecked" to synthetic vars
      def flags: List[Long] = List(Flags.TRANS_FLAG) filter (sym hasFlag _)

      def castedTo(headType: Type) =
        if (tpe =:= headType) this
        else new Scrutinee(newVar(pos, headType, flags))

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

      def headType = head.typeToMatch
      def isCaseHead = head.isCaseClass
      private val dummyCount = if (isCaseHead) headType.typeSymbol.caseFieldAccessors.length else 0
      def dummies = emptyPatterns(dummyCount)

      def apply(i: Int): Pattern = ps(i)
      def pzip() = ps.zipWithIndex
      def pzip[T](others: List[T]) = ps zip others

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
      def pats: PatternMatch
      def rest: Rep
      lazy val PatternMatch(scrut, patterns) = pats
      lazy val head = pats.head

      /** Creates Some(fail rule) even if xs == Nil. */
      def mkFail(xs: List[Row]): Option[Rep] = Some(make(scrut.sym :: rest.tvars, xs))

      /** Returns None if xs == Nil, Some(fail rule) otherwise. */
      def mkFailOpt(xs: List[Row]): Option[Rep] = if (xs.isEmpty) None else mkFail(xs)

      /** Splices scrutinee's symbol in between the given lists */
      def mkNewRep(pre: List[Symbol], post: List[Symbol], rows: List[Row]) =
        make(pre ::: scrut.sym :: post, rows)

      /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
      def tree(): Tree

      override def toString =
        "Rule/%s (%s =^= %s)".format(getClass.getSimpleName, scrut, head)
    }

    case class ErrorRule() extends RuleApplication {
      def pats: PatternMatch = impossible
      def rest: Rep = impossible
      final def tree() = failTree
    }

    /** {case ... if guard => bx} else {guardedRest} */
    /** VariableRule: The top-most rows has only variable (non-constructor) patterns. */
    case class VariableRule(subst: Bindings, guard: Guard, guardedRest: Rep, bx: Int) extends RuleApplication {
      def pats: PatternMatch = impossible
      def rest: Rep = guardedRest

      final def tree(): Tree = {
        def body      = requestBody(bx, subst)
        def guardTest = IF (guard.duplicate.tree) THEN body ELSE guardedRest.toTree
        implicit val ctx = context

        typer typed(
          if (guard.isEmpty) body
          else squeezedBlock(subst.infoForAll.patternValDefs, guardTest)
        )
      }
    }

    /** Mixture rule for all literal ints (and chars) i.e. hopefully a switch
     *  will be emitted on the JVM.
     */
    class MixLiteralInts(val pats: PatternSwitch, val rest: Rep) extends RuleApplication
    {
      val literals = pats.ps
      val defaultPattern = pats.defaultPattern

      // bound vars and rows for default pattern (only one row, but a list is easier to use later)
      val (defaultVars, defaultRows) = defaultPattern match {
        case None                 => (Nil, Nil)
        case Some(Pattern(_, vs)) => (vs, List((rest rows literals.size).rebind2(vs, scrut.sym)))
      }
      // literalMap is a map from each literal to a list of row indices.
      // varMap is a list from each literal to a list of the defined vars.
      val (literalMap, varMap) = {
        val tags    = literals map (_.intValue)
        val varMap  = tags zip (literals map (_.definedVars))
        val litMap  =
          tags.zipWithIndex.reverse.foldLeft(IntMap.empty[List[Int]]) {
            // we reverse before the fold so the list can be built with ::
            case (map, (tag, index)) => map.updated(tag, index :: map.getOrElse(tag, Nil))
          }

        (litMap, varMap)
      }

      final def tree(): Tree = {
        // Just a demo of breakIf
        // Interpreter.breakIf(true, "lits" -> literals, "mixlit" -> this, literalMap)

        def bindVars(Tag: Int, orig: Bindings): Bindings = {
          def myBindVars(rest: List[(Int, List[Symbol])], bnd: Bindings): Bindings = rest match {
            case Nil => bnd
            case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs, scrut.sym))
            case (_,  vs)::xs => myBindVars(xs, bnd)
          }
          myBindVars(varMap, orig)
        }

        // creates a row transformer for injecting the default case bindings at a given index
        def addDefaultVars(index: Int): Row => Row =
          if (defaultVars.isEmpty) identity
          else (r: Row) => r.rebind2(pats(index).boundVariables, scrut.sym)

        val cases =
          for ((tag, indices) <- literalMap.toList) yield {
            val newRows = indices map (i => addDefaultVars(i)(rest rows i))
            val r       = make(rest.tvars, newRows ::: defaultRows)
            val r2      = make(r.tvars, r.rows map (x => x rebind bindVars(tag, x.subst)))

            CASE(Literal(tag)) ==> r2.toTree
          }

        val defaultTree       = make(rest.tvars, defaultRows).toTree
        def casesWithDefault  = cases ::: List(CASE(WILD(IntClass.tpe)) ==> defaultTree)

        cases match {
          case List(CaseDef(lit, _, body))  =>
            // only one case becomes if/else
            IF (scrut.id MEMBER_== lit) THEN body ELSE defaultTree
          case _                            =>
            // otherwise cast to an Int if necessary and run match
            val target: Tree = if (!scrut.tpe.isInt) scrut.id DOT nme.toInt else scrut.id
            target MATCH (casesWithDefault: _*)
        }
      }
    }

    /** mixture rule for unapply pattern
     */
    class MixUnapply(val pats: PatternMatch, val rest: Rep, typeTest: Boolean) extends RuleApplication {
      // Note: trailingArgs is not necessarily Nil, because unapply can take implicit parameters.
      lazy val ua @ UnApply(app, args) = head.tree
      lazy val Apply(fxn, _ :: trailingArgs) = app

      object sameUnapplyCall {
        private def sameFunction(fn1: Tree) = fxn.symbol == fn1.symbol && (fxn equalsStructure fn1)

        def unapply(p: Pattern) = condOpt(p) {
          case Pattern(UnApply(Apply(fn1, _), args), _) if sameFunction(fn1)  => args
        }
      }

    /** returns (un)apply-call, success-rep, optional fail-rep */
      final def getTransition(): Branch[UnapplyCall] = {
        val unapplyRes  = newVar(ua.pos, app.tpe, scrut.flags)
        val rhs         = Apply(fxn, scrut.id :: trailingArgs) setType unapplyRes.tpe
        val zipped      = pats pzip rest.rows
        val nrowsOther  = zipped.tail flatMap {
          case (sameUnapplyCall(_), _)  => Nil
          case (pat, r)                 => List(r insert pat)
        }

        def mkTransition(vdefs: List[Tree], ntemps: List[Symbol], nrows: List[Row]) =
          Branch(
            UnapplyCall(typedValDef(unapplyRes, rhs), vdefs),
            mkNewRep(ntemps, rest.tvars, nrows),
            mkFailOpt(nrowsOther)
          )

        // Second argument is number of dummies to prepend in the default case
        def mkNewRows(sameFilter: (List[Tree]) => List[Tree], dum: Int) =
          for ((pat, r) <- zipped) yield pat match {
            case sameUnapplyCall(args)  => r.insert2(toPats(sameFilter(args)) ::: List(NoPattern), pat.boundVariables, scrut.sym)
            case _                      => r insert (emptyPatterns(dum) ::: List(pat))
          }
        def mkGet(s: Symbol) = typedValDef(s, fn(ID(unapplyRes), nme.get))
        def mkVar(tpe: Type) = newVar(ua.pos, tpe, scrut.flags)

        // 0 args => Boolean, 1 => Option[T], >1 => Option[? <: ProductN[T1,...,Tn]]
        args.length match {
          case 0  =>
            mkTransition(Nil, Nil, mkNewRows((xs) => Nil, 0))

          case 1 =>
            val vsym  = mkVar(app.tpe typeArgs 0)
            val nrows = mkNewRows(xs => List(xs.head), 1)
            val vdef  = mkGet(vsym)

            mkTransition(List(vdef), List(vsym), nrows)

          case _ =>
            val uresGet   = mkVar(app.tpe typeArgs 0)
            val vdefHead  = mkGet(uresGet)
            val ts        = getProductArgs(uresGet.tpe).get
            val nrows     = mkNewRows(identity, ts.size)

            val (vdefs: List[Tree], vsyms: List[Symbol]) = List.unzip(
              for ((vtpe, i) <- ts.zipWithIndex) yield {
                val vchild  = mkVar(vtpe)
                val accSym  = productProj(uresGet, i+1)
                val rhs     = typer typed fn(ID(uresGet), accSym)

                (typedValDef(vchild, rhs), vchild)
              })

            mkTransition(vdefHead :: vdefs, vsyms, nrows)
        }
      } /* def getTransition(...) */

      final def tree() = {
        val Branch(UnapplyCall(uacall, vdefs), srep, frep) = this.getTransition
        val succ = srep.toTree
        val fail = frep map (_.toTree) getOrElse (failTree)
        val cond =
          if (uacall.symbol.tpe.isBoolean) ID(uacall.symbol)
          else uacall.symbol IS_DEFINED

        typer typed squeezedBlock(
          List(handleOuter(uacall)),
          IF (cond) THEN squeezedBlock(vdefs, succ) ELSE fail
        )
      }
    }

    /** handle sequence pattern and ArrayValue (but not star patterns)
     */
    sealed class MixSequence(val pats: PatternMatch, val rest: Rep) extends RuleApplication {
      // Called 'pivot' since it's the head of the column under consideration in the mixture rule.
      val pivot @ SequencePattern(av @ ArrayValue(_, _)) = head
      private def pivotLen = pivot.nonStarLength

      /** array elements except for star (if present) */
      protected def nonStarElems(x: ArrayValue) =
        if (isRightIgnoring(x)) x.elems.init else x.elems

      final def removeStar(xs: List[Pattern]): List[Pattern] =
        xs.init ::: List(Pattern(makeBind(xs.last.boundVariables, WILD(scrut.seqType))))

      def mustCheck(first: Pattern, next: Pattern): Boolean = {
        if (first.tree eq next.tree)
          return false

        !(first completelyCovers next)
      }

      def getSubPatterns(x: Pattern): Option[List[Pattern]] = condOpt(x.tree) {
        case av @ ArrayValue(_, xs) if nonStarElems(av).length == pivotLen =>
          val (star1, star2) = (pivot.hasStar, isRightIgnoring(av))

          (star1, star2) match {
            case (true, true)   => removeStar(toPats(xs)) ::: List(NoPattern)
            case (true, false)  => toPats(xs ::: List(gen.mkNil, EmptyTree))
            case (false, true)  => removeStar(toPats(xs))
            case (false, false) => toPats(xs) ::: List(NoPattern)
          }
        case av @ ArrayValue(_, xs) if pivot.hasStar && isRightIgnoring(av) && xs.length-1 < pivotLen =>
          emptyPatterns(pivotLen + 1) ::: List(x)

        case EmptyTree | WILD() =>
          emptyPatterns(pivot.elemPatterns.length + 1)
      }

      def makeSuccRep(vs: List[Symbol], tail: Symbol, nrows: List[Row]) = {
        val ssym = if (pivot.hasStar) List(scrut.sym) else Nil

        make(List(vs, List(tail), ssym, rest.tvars).flatten, nrows)
      }

      case class TransitionContext(f: TreeFunction2)

      // context (to be used in IF), success and failure Rep
      def getTransition(): Branch[TransitionContext] = {
        assert(scrut.tpe <:< head.tpe, "fatal: %s is not <:< %s".format(scrut, head.tpe))

        val vs                  = pivot.nonStarPatterns map (x => newVar(x.tree.pos, scrut.elemType))
        lazy val tail           = scrut.newVarOfSeqType
        lazy val lastBinding    = typedValDef(tail, scrut.id DROP vs.size)
        def elemAt(i: Int)      = typer typed ((scrut.id DOT (scrut.tpe member nme.apply))(LIT(i)))

        val bindings =
          (vs.zipWithIndex map tupled((v, i) => typedValDef(v, elemAt(i)))) ::: List(lastBinding)

        val (nrows, frows): (List[Option[Row]], List[Option[Row]]) = List.unzip(
          for ((c, rows) <- pats pzip rest.rows) yield getSubPatterns(c) match {
            case Some(ps) => (Some(rows insert ps), if (mustCheck(head, c)) Some(rows insert c) else None)
            case None     => (None, Some(rows insert c))
          }
        )

        val succ = makeSuccRep(vs, tail, nrows flatMap (x => x))
        val fail = mkFail(frows flatMap (x => x))
        def transition = (thenp: Tree, elsep: Tree) =>
          IF (getPrecondition(scrut.id, pivot.nonStarLength)) THEN squeezedBlock(bindings, thenp) ELSE elsep

        Branch(TransitionContext(transition), succ, fail)
      }

      protected def lengthCheck(tree: Tree, len: Int, op: TreeFunction2) = {
        def compareOp = head.tpe member nme.lengthCompare  // symbol for "lengthCompare" method
        def cmpFunction(t1: Tree) = op((t1.duplicate DOT compareOp)(LIT(len)), ZERO)
        // first ascertain lhs is not null: bug #2241
        typer typed nullSafe(cmpFunction _, FALSE)(tree)
      }

      // precondition for matching
      protected def getPrecondition(tree: Tree, lengthArg: Int) =
        if (pivot.hasStar) lengthCheck(tree, lengthArg, _ ANY_>= _)   // seq length >= pattern length
        else lengthCheck(tree, lengthArg, _ MEMBER_== _)              // seq length == pattern length

      final def tree() = {
        val Branch(TransitionContext(transition), succ, Some(fail)) = this.getTransition
        transition(succ.toTree, fail.toTree)
      }
    }

    // @todo: equals test for same constant
    class MixEquals(val pats: PatternMatch, val rest: Rep) extends RuleApplication {
      /** condition (to be used in IF), success and failure Rep */
      final def getTransition(): (Branch[Tree], Symbol) = {
        val value = {
          // val TypeRef(_,_,List(arg)) = head.tpe
          val arg = decodedEqualsType(head.tpe)

          arg match {
            case SingleType(pre, sym) => REF(pre, sym)
            case PseudoType(o)        => o.duplicate
          }
        }

        val label       = owner.newLabel(scrut.pos, newName(scrut.pos, "failCont%")) // warning, untyped - typed in tree()
        val succ        = List(
          rest.rows.head.insert2(List(NoPattern), head.boundVariables, scrut.sym),
          Row(emptyPatterns(1 + rest.tvars.length), NoBinding, NoGuard, shortCut(label))
        )

        // todo: optimize if no guard, and no further tests
        val fail    = mkFail(List.map2(rest.rows.tail, pats.tail)(_ insert _))
        val action  = typer typed (scrut.id MEMBER_== value)

        (Branch(action, mkNewRep(Nil, rest.tvars, succ), fail), label)
      }

      final def tree() = {
        val (Branch(cond, srep, Some(frep)), failLabel) = this.getTransition
        val fail  = typer typed frep.toTree
        failLabel setInfo MethodType(Nil, fail.tpe)

        typer typed(
          IF (handleOuter(cond)) THEN srep.toTree ELSE LabelDef(failLabel, Nil, fail)
        )
      }
      override def toString() = "MixEquals(%s == %s)".format(scrut, head)
    }

    /** mixture rule for type tests
    **/
    class MixTypes(val pats: PatternMatch, val rest: Rep) extends RuleApplication {
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
        for ((pattern, j) <- pats.pzip()) yield {
          // scrutinee, head of pattern group
          val (s, p) = (pattern.tpe, pats.headType)

          def sMatchesP = matches(s, p)
          def pMatchesS = matches(p, s)

          def alts[T](yes: T, no: T): T = if (p =:= s) yes else no
          def isObjectTest              = pattern.isObject && (p =:= pattern.typeToMatch)

          lazy val dummy          = (j, pats.dummies)
          lazy val pass           = (j, pattern)
          lazy val subs           = (j, pattern subpatterns pats)

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
            case Pattern(Typed(pp @ Pattern(_: UnApply, _), _), _) if sMatchesP => (Pattern(pp), dummy, None)                 // (3)
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

      override def toString = {
        val msgs = List(
          "moreSpecific: " + pp(moreSpecific),
          "    subsumed: " + pp(subsumed),
          "   remaining: " + pp(remaining)
        )

        super.toString() + "\n" + indentAll(msgs)
      }

      /** returns casted symbol, success matrix and optionally fail matrix for type test on the top of this column */
      final def getTransition(): Branch[Scrutinee] = {
        val casted = scrut castedTo pats.headType

        val isAnyMoreSpecific = moreSpecific exists (x => !x.isEmpty)

        def mkZipped    = moreSpecific zip subsumed map { case (mspat, (j, pats)) => (j, mspat :: pats) }

        val (subtests, subtestVars) =
          if (isAnyMoreSpecific)  (mkZipped, List(casted.sym))
          else                    (subsumed, Nil)

        val newRows =
          for ((j, ps) <- subtests) yield
            (rest rows j).insert2(ps, pats(j).boundVariables, casted.sym)

        Branch(
          casted,
          // succeeding => transition to translate(subsumed) (taking into account more specific)
          make(subtestVars ::: casted.accessorVars ::: rest.tvars, newRows),
          // fails      => transition to translate(remaining)
          mkFailOpt(remaining map tupled((p1, p2) => rest rows p1 insert p2))
        )
      }

      // temporary checks so we're less crashy while we determine what to implement.
      def checkErroneous(scrut: Scrutinee): Type = {
        scrut.tpe match {
          case tpe @ ThisType(_) if tpe.termSymbol == NoSymbol        =>
            cunit.error(scrut.pos, "self type test in anonymous class forbidden by implementation.")
            ErrorType
          case x => x
        }
      }

      final def tree(): Tree = {
        val Branch(casted, srep, frep) = this.getTransition
        val castedTpe = checkErroneous(casted)
        val cond = condition(castedTpe, scrut)
        val succ = srep.toTree
        val fail = frep map (_.toTree) getOrElse (failTree)

        // dig out case field accessors that were buried in (***)
        val cfa         = if (pats.isCaseHead) casted.accessors else Nil
        val caseTemps   = srep.tvars match { case x :: xs if x == casted.sym => xs ; case x => x }
        def castedScrut = typedValDef(casted.sym, scrut.id AS_ANY castedTpe)
        def needCast    = if (casted.sym ne scrut.sym) List(castedScrut) else Nil

        val vdefs       = needCast ::: (
          for ((tmp, accessor) <- caseTemps zip cfa) yield
            typedValDef(tmp, typer typed fn(casted.id, accessor))
        )

        typer typed (IF (cond) THEN squeezedBlock(vdefs, succ) ELSE fail)
      }
    }

    /*** States, Rows, Etc. ***/

    case class Row(pat: List[Pattern], subst: Bindings, guard: Guard, bx: Int) {
      if (pat exists (p => !p.isDefault))
        traceCategory("Row", "%s", pp(pat))

      def insert(h: Pattern)              = copy(pat = h :: pat)
      def insert(hs: List[Pattern])       = copy(pat = hs ::: pat)    // prepends supplied pattern
      def replace(hs: List[Pattern])      = copy(pat = hs)            // substitutes for patterns
      def rebind(b: Bindings)             = copy(subst = b)           // substitutes for bindings

      def rebind2(vs: Iterable[Symbol], tvars: Symbol) =
        copy(subst = subst.add(vs, tvars))

      def insert2(hs: List[Pattern], vs: Iterable[Symbol], tvars: Symbol) =             // prepends and prepends
        copy(pat = hs ::: pat, subst = subst.add(vs, tvars))

      /** returns true if the patterns in this rows cover a type symbols "combination" and there is no guard
       *  @param comb pairs of (column index, type symbol)
       */
      def covers(combos: List[Combo]) =
        guard.isEmpty && (combos forall (c => c isCovered pat(c.index)))

      // returns this rows with alternatives expanded
      def expandAlternatives(classifyPat: (Pattern, Int) => Pattern): List[Row] = {
        // classify all the top level patterns - alternatives come back unaltered
        val newPats: List[Pattern] = List.map2(pat, pat.indices.toList)(classifyPat)
        // expand alternatives if any are present
        (newPats indexWhere (_.isAlternative)) match {
          case -1     => List(replace(newPats))
          case index  =>
            val (prefix, alts :: suffix) = newPats splitAt index
            // make a new row for each alternative, with it spliced into the original position
            extractBindings(alts.boundTree) map (x => replace(prefix ::: Pattern(x) :: suffix))
        }
      }
      override def toString() = pp(bx -> (pat, subst))
    }

    object ExpandedMatrix {
      def unapply(x: ExpandedMatrix) = Some((x.rows, x.targets))
      def apply(rows: List[Row], targets: List[FinalState]) = new ExpandedMatrix(rows, targets)
    }
    class ExpandedMatrix(val rows: List[Row], val targets: List[FinalState]) {
      require(rows.size == targets.size)

      override def toString() =
        "ExpandedMatrix(%d)".format(rows.size) + pp(rows zip targets, true)
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

      def createLabelBody(name: String, args: List[Symbol], vdefs: List[Tree]) = {
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

      def getLabelBody(idents: List[Tree], vdefs: List[Tree]): Tree = {
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

    case class Combo(index: Int, sym: Symbol) {
      // is this combination covered by the given pattern?
      def isCovered(p: Pattern) = {
        def cmpSymbols(t1: Type, t2: Type)  = t1.typeSymbol eq t2.typeSymbol
        def coversSym = {
          val tpe = decodedEqualsType(p.tpe)
          lazy val lmoc = sym.linkedModuleOfClass
          val symtpe =
            if ((sym hasFlag Flags.MODULE) && (lmoc ne NoSymbol))
              singleType(sym.tpe.prefix, lmoc)   // e.g. None, Nil
            else sym.tpe

          (tpe.typeSymbol == sym) ||
          (symtpe <:< tpe) ||
          (symtpe.parents exists (x => cmpSymbols(x, tpe))) || // e.g. Some[Int] <: Option[&b]
          ((tpe.prefix memberType sym) <:< tpe)  // outer, see combinator.lexical.Scanner
        }

        cond(p.tree) {
          case _: UnApply | _: ArrayValue => true
          case x                          => p.isDefault || coversSym
        }
      }
    }
    case class Branch[T](action: T, succ: Rep, fail: Option[Rep])
    case class UnapplyCall(ua: Tree, args: List[Tree])

    case class Rep(val tvars: List[Symbol], val rows: List[Row]) {
      import Flags._

      /** Converts this to a tree - recursively acquires subreps. */
      final def toTree(): Tree = this.applyRule.tree()

      private def toUse(s: Symbol) =
         (s hasFlag MUTABLE) &&                 // indicates that have not yet checked exhaustivity
        !(s hasFlag TRANS_FLAG) &&              // indicates @unchecked
         (s.tpe.typeSymbol hasFlag SEALED)

      // the superclass is taken if it is not abstract
      private def countSuper(s: Symbol): Set[Symbol]  = if (s hasFlag ABSTRACT) emptySymbolSet else Set(s)
      private def countSymbol(s: Symbol): Set[Symbol] = candidates(s) ++ countSuper(s)
      private def candidates(s: Symbol): Set[Symbol]  =
        if (s hasFlag SEALED) s.children flatMap countSymbol
        else emptySymbolSet

      private def setsToCombine: List[(Int, Set[Symbol])] =
        for ((sym, i) <- tvars.zipWithIndex ; if toUse(sym)) yield {
          sym resetFlag MUTABLE
          (i, candidates(sym.tpe.typeSymbol))
        }

      // computes cartesian product, keeps indices available
      private def combine(colcom: List[(Int, Set[Symbol])]): List[List[Combo]] = colcom match {
        case Nil              => Nil
        case (i, syms) :: Nil => syms.toList map (s => List(Combo(i, s)))
        case (i, syms) :: cs  => for (s <- syms.toList; rest <- combine(cs)) yield Combo(i, s) :: rest
      }

      /** Applying the rule will result in one of:
        *
        *   VariableRule - if all patterns are default patterns
        *    MixtureRule - if one or more patterns are not default patterns
        *      ErrorRule - if there are no rows remaining
        */
      final def applyRule(): RuleApplication = {
        def dropIndex[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop (n + 1))

        lazy val Row(pats, subst, guard, index) = rows.head
        lazy val guardedRest        = if (guard.isEmpty) NoRep else make(tvars, rows.tail)
        lazy val (defaults, others) = pats span (_.isDefault)
        lazy val ndIndex            = defaults.size   // index of non-default pattern in pats

        if (rows.isEmpty) ErrorRule()
        else if (others.isEmpty) {
          /** top-most rows contains only variables/wildcards */
          val binding = (defaults map (_.boundVariables) zip tvars) .
            foldLeft(subst)((b, pair) => b.add(pair._1, pair._2))

          VariableRule(binding, guard, guardedRest, index)
        }
        else {
          /** cut out the column (px) containing the non-default pattern. */
          val rpat      = others.head
          val vs        = rpat.boundVariables
          val column    = rpat :: (rows.tail map (_ pat ndIndex))
          val restTemp  = dropIndex(tvars, ndIndex)
          val restRows  = rows map (r => r replace dropIndex(r.pat, ndIndex))

          MixtureRule(new Scrutinee(tvars(ndIndex)), column, make(restTemp, restRows))
        }
      }

      def checkExhaustive: this.type = {
        val allcomb = combine(setsToCombine)
        if (allcomb forall (combo => rows exists (_ covers combo)))
          return this

        // if we reach here, patterns were not exhaustive
        def mkPad(xs: List[Combo], i: Int): String = xs match {
          case Nil                    => pad("*")
          case Combo(j, sym) :: rest  => if (j == i) pad(sym.name.toString) else mkPad(rest, i)
        }
        def mkMissingStr(open: List[Combo]) =
          "missing combination " + tvars.indices.map(mkPad(open, _)).mkString + "\n"

        val missingCombos = allcomb filter (open => rows.forall(!_.covers(open))) map mkMissingStr
        cunit.warning(tvars.head.pos, "match is not exhaustive!\n" + missingCombos.mkString)
        this
      }

      override def toString() = {
        if (tvars.size == 0) "Rep(%d) = %s".format(rows.size, pp(rows))
        else "Rep(%dx%d)\n  %s\n  %s".format(tvars.size, rows.size, pp(tvars), pp(rows))
      }

      private def pad(s: Any): String = pad(s match {
        case x: Tree    => treeToString(x)
        case x          => x.toString
      })
      private val NPAD = 15
      private def pad(s: String): String = "%%%ds" format (NPAD-1) format s
    }

    val NoRep = Rep(Nil, Nil)
    /** Expands the patterns recursively. */
    final def expand(roots: List[Symbol], cases: List[CaseDef]): ExpandedMatrix = {
      val (rows, finals) = List.unzip(
        for ((CaseDef(pat, guard, body), index) <- cases.zipWithIndex) yield {
          def mkRow(ps: List[Tree]) = Row(toPats(ps), NoBinding, Guard(guard), index)

          val pattern = Pattern(pat)
          val row = mkRow(pat match {
            case x if roots.length <= 1 => List(x)
            case Apply(_, args)         => args
            case WILD()                 => emptyTrees(roots.length)
          })

          (row, FinalState(index, body, pattern.definedVars))
        }
      )

      tracing("Expanded", new ExpandedMatrix(rows, finals))
    }

    /** returns the condition in "if (cond) k1 else k2"
     */
    final def condition(tpe: Type, scrut: Scrutinee): Tree = {
      assert(scrut.isDefined)
      val cond = handleOuter(condition(tpe, scrut.id))

      if (!needsOuterTest(tpe, scrut.tpe, owner)) cond
      else addOuterCondition(cond, tpe, scrut.id, handleOuter)
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
    final def addOuterCondition(cond: Tree, tpe2test: Type, scrut: Tree, handleOuter: TreeFunction1) = {
      val theRef = handleOuter(tpe2test match {
        case TypeRef(NoPrefix, _, _)          => abort("assertion failed: NoPrefix")
        case TypeRef(ThisType(clazz), _, _)   => THIS(clazz)
        case TypeRef(prefix, _, _)            => REF(prefix.prefix, prefix.termSymbol)
      })

      outerAccessor(tpe2test.typeSymbol) match {
        case NoSymbol => ifDebug(cunit.warning(scrut.pos, "no outer acc for " + tpe2test.typeSymbol)) ; cond
        case outerAcc => cond AND (((scrut AS_ANY tpe2test) DOT outerAcc)() ANY_EQ theRef)
      }
    }
  }
}
