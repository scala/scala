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
package tools.nsc.transform.patmat

import scala.collection.immutable.ArraySeq
import scala.collection.{IterableOps, mutable}
import scala.reflect.internal.util.Collections._
import scala.reflect.internal.util.HashSet
import scala.tools.nsc.transform.patmat.Logic.LogicLinkedHashSet

trait Logic extends Debugging {
  import global._

  private def max(xs: Seq[Int]) = if (xs.isEmpty) 0 else xs.max
  private def alignedColumns(cols: Seq[Any]): Seq[String] = {
    def toString(x: Any) = if (x == null) "" else x.toString
    if (cols.isEmpty || cols.tails.isEmpty) cols map toString
    else {
      val colLens = cols map (c => toString(c).length)
      val maxLen = max(colLens)
      val avgLen = colLens.sum/colLens.length
      val goalLen = maxLen min avgLen*2
      def pad(s: String) = {
        val toAdd = ((goalLen - s.length) max 0) + 2
        (" " * (toAdd/2)) + s + (" " * (toAdd/2 + (toAdd%2)))
      }
      cols map (x => pad(toString(x)))
    }
  }

  def alignAcrossRows(xss: List[List[Any]], sep: String, lineSep: String = "\n"): String = {
    val maxLen = max(xss map (_.length))
    val padded = xss map (xs => xs ++ List.fill(maxLen - xs.length)(null))
    padded.transpose.map(alignedColumns).transpose map (_.mkString(sep)) mkString(lineSep)
  }

  // ftp://ftp.cis.upenn.edu/pub/cis511/public_html/Spring04/chap3.pdf
  // http://users.encs.concordia.ca/~ta_ahmed/ms_thesis.pdf
  // propositional logic with constants and equality
  trait PropositionalLogic {
    type Type
    type Tree

    sealed abstract class Prop
    final case class Eq(p: Var, q: Const) extends Prop

    type Const

    type TypeConst <: Const
    def TypeConst: TypeConstExtractor
    trait TypeConstExtractor {
      def apply(tp: Type): Const
    }

    type ValueConst <: Const
    def ValueConst: ValueConstExtractor
    trait ValueConstExtractor {
      def apply(p: Tree): Const
    }

    val NullConst: Const

    type Var <: AbsVar
    val Var: VarExtractor
    trait VarExtractor {
      def apply(x: Tree): Var
      def unapply(v: Var): Some[Tree]
    }

    def uncheckedWarning(pos: Position, msg: String, site: Symbol): Unit

    def reportWarning(message: String): Unit

    // resets hash consing -- only supposed to be called by TreeMakersToProps
    def prepareNewAnalysis(): Unit

    trait AbsVar {
      // indicate we may later require a prop for V = C
      def registerEquality(c: Const): Unit

      // call this to indicate null is part of the domain
      def registerNull(): Unit

      // can this variable be null?
      def mayBeNull: Boolean

      // compute the domain and return it (call registerNull first!)
      def domainSyms: Option[Set[Sym]]

      def groupedDomains: List[Set[Sym]]

      // the symbol for this variable being equal to its statically known type
      // (only available if registerEquality has been called for that type before)
      def symForStaticTp: Option[Sym]

      // for this var, call it V, turn V = C into the equivalent proposition in boolean logic
      // registerEquality(c) must have been called prior to this call
      // in fact, all equalities relevant to this variable must have been registered
      def propForEqualsTo(c: Const): Prop

      // populated by registerEquality
      // once implications has been called, must not call registerEquality anymore
      def implications: List[(Sym, List[Sym], List[Sym])]
    }

    // Using LogicLinkedHashSet (a custom mutable.LinkedHashSet subclass) to ensure deterministic exhaustivity
    // messages. immutable.ListSet was too slow (concatenate cost? scala/bug#12499).

    // would be nice to statically check whether a prop is equational or pure,
    // but that requires typing relations like And(x: Tx, y: Ty) : (if(Tx == PureProp && Ty == PureProp) PureProp else Prop)
    final case class And(ops: LogicLinkedHashSet[Prop]) extends Prop
    object And {
      def apply(ps: Prop*)           = create(ps)
      def create(ps: Iterable[Prop]) = new And(ps.to(LogicLinkedHashSet))
    }

    final case class Or(ops: LogicLinkedHashSet[Prop]) extends Prop
    object Or {
      def apply(ps: Prop*)           = create(ps)
      def create(ps: Iterable[Prop]) = new Or(ps.to(LogicLinkedHashSet))
    }

    final case class Not(a: Prop) extends Prop

    // mutually exclusive (i.e., not more than one symbol is set)
    final case class AtMostOne(ops: List[Sym]) extends Prop

    case object True extends Prop
    case object False extends Prop

    // symbols are propositions
    final class Sym private[PropositionalLogic] (val variable: Var, val const: Const) extends Prop {

      override def equals(other: scala.Any): Boolean = other match {
        case that: Sym => this.variable == that.variable &&
          this.const == that.const
        case _         => false
      }

      override def hashCode(): Int = {
        variable.hashCode * 41 + const.hashCode
      }

      private val id: Int = Sym.nextSymId

      override def toString = s"$variable=$const#$id"
    }

    object Sym {
      private val uniques: HashSet[Sym] = new HashSet("uniques", 512)
      def apply(variable: Var, const: Const): Sym = {
        val newSym = new Sym(variable, const)
        (uniques findEntryOrUpdate newSym)
      }
      def nextSymId = {_symId += 1; _symId}; private[this] var _symId = 0
      implicit val SymOrdering: Ordering[Sym] = Ordering.by(_.id)
    }

    def /\(props: Iterable[Prop]) = props match {
      case _ if props.isEmpty     => True
      case _ if props.sizeIs == 1 => props.head
      case _                      => And.create(props)
    }

    def \/(props: Iterable[Prop]) = props match {
      case _ if props.isEmpty     => False
      case _ if props.sizeIs == 1 => props.head
      case _                      => Or.create(props)
    }

    /**
     * Simplifies propositional formula according to the following rules:
     * - eliminate double negation (avoids unnecessary Tseitin variables)
     * - flatten trees of same connectives (avoids unnecessary Tseitin variables)
     * - removes constants and connectives that are in fact constant because of their operands
     * - eliminates duplicate operands
     * - convert formula into NNF: all sub-expressions have a positive polarity
     * which makes them amenable for the subsequent Plaisted transformation
     * and increases chances to figure out that the formula is already in CNF
     *
     * Complexity: DFS over formula tree
     *
     * See https://www.decision-procedures.org/slides/propositional_logic-2x3.pdf
     */
    def simplify(f: Prop): Prop = {

      def hasImpureAtom(ops0: Iterable[Prop]): Boolean = {
        // HOT method, imperative rewrite of:
        // ops.combinations(2).exists {
        //   case Seq(a, Not(b)) if a == b => true
        //   case Seq(Not(a), b) if a == b => true
        //   case _                        => false
        // }

        def checkPair(a: Prop, b: Prop): Boolean = {
          b match {
            case Not(b) if a == b => true
            case _ =>
              a match {
                case Not(a) if a == b => true
                case _ => false
              }
          }
        }
        val size = ops0.size
        if (size > 10) false // limit size to avoid blow up
        else if (size < 2) false // no combinations
        else if (size == 2) { // Specialized versions for size 2+3
          val it = ops0.iterator
          val result = checkPair(it.next(), it.next())
          assert(!it.hasNext, "iterator must be empty")
          result
        } else if (size == 3) {
          val it = ops0.iterator
          val a = it.next()
          val b = it.next()
          val c = it.next()
          assert(!it.hasNext, "iterator must be empty")
          checkPair(a, b) || checkPair(a, c) || checkPair(b, c)
        } else {
          val ops = new Array[Prop](size)
          @annotation.unused val copied = ops0.copyToArray(ops)
          //assert(copied == ops.length, "")
          var i = 0
          val len = ops.length
          while (i < len - 1) {
            var j = i + 1
            while (j < len) {
              if (checkPair(ops(i), ops(j))) return true
              j += 1
            }
            i += 1
          }
          false
        }
      }

      def mapConserve[CC[X] <: IterableOps[X, CC, CC[X]], A <: AnyRef](s: CC[A])(f: A => A): CC[A] = {
        var changed = false
        val s1 = s.map {a =>
          val a1 = f(a)
          if (a1 ne a) changed = true
          a1
        }
        if (changed) s1 else s
      }

      // push negation inside formula
      def negationNormalFormNot(p: Prop): Prop = p match {
        case And(ops)         => Or(mapConserve(ops)(negationNormalFormNot)) // De Morgan
        case Or(ops)          => And(mapConserve(ops)(negationNormalFormNot)) // De Morgan
        case Not(p)           => negationNormalForm(p)
        case True             => False
        case False            => True
        case s: Sym           => Not(s)
        case Eq(_, _)         => Not(p)
        case p @ AtMostOne(_) => Not(p)
      }

      def negationNormalForm(p: Prop): Prop = p match {
        case And(ops)     =>
          val ops1 = mapConserve(ops)(negationNormalForm)
          if (ops1 eq ops) p else And(ops1)
        case Or(ops)      =>
          val ops1 = mapConserve(ops)(negationNormalForm)
          if (ops1 eq ops) p else Or(ops1)
        case Not(negated) => negationNormalFormNot(negated)
        case True
             | False
             | (_: Sym)
             | (_: Eq)
             | (_: AtMostOne)   => p
      }

      def simplifyAnd(ps: Iterable[Prop]): Prop = {
        // recurse for nested And (pulls all Ands up)
        // build up Set in order to remove duplicates
        val props = LogicLinkedHashSet.empty[Prop]
        for (prop <- ps) {
          simplifyProp(prop) match {
            case True    => // ignore `True`
            case And(fv) => fv.foreach(props += _)
            case f       => props += f
          }
        }

        if (props.contains(False) || hasImpureAtom(props)) False
        else /\(props)
      }

      def simplifyOr(ps: Iterable[Prop]): Prop = {
        // recurse for nested Or (pulls all Ors up)
        // build up Set in order to remove duplicates
        val props = LogicLinkedHashSet.empty[Prop]
        for (prop <- ps) {
          simplifyProp(prop) match {
            case False  => // ignore `False`
            case Or(fv) => props ++= fv
            case f      => props += f
          }
        }

        if (props.contains(True) || hasImpureAtom(props)) True
        else \/(props)
      }

      def simplifyProp(p: Prop): Prop = p match {
        case And(ps)     => simplifyAnd(ps)
        case Or(ps)      => simplifyOr(ps)
        case Not(Not(a)) => simplify(a)
        case Not(p)      => Not(simplify(p))
        case p           => p
      }

      val nnf = negationNormalForm(f)
      simplifyProp(nnf)
    }

    trait PropTraverser {
      def apply(x: Prop): Unit = x match {
        case And(ops) => ops foreach apply
        case Or(ops) => ops foreach apply
        case Not(a) => apply(a)
        case Eq(a, b) => applyVar(a); applyConst(b)
        case s: Sym => applySymbol(s)
        case AtMostOne(ops) => ops.foreach(applySymbol)
        case _ =>
      }
      def applyVar(x: Var): Unit = {}
      def applyConst(x: Const): Unit = {}
      def applySymbol(x: Sym): Unit = {}
    }

    def gatherVariables(p: Prop): collection.Set[Var] = {
      val vars = new LogicLinkedHashSet[Var]()
      (new PropTraverser {
        override def applyVar(v: Var) = vars += v
      })(p)
      vars
    }

    def gatherSymbols(p: Prop): collection.Set[Sym] = {
      val syms = new LogicLinkedHashSet[Sym]()
      (new PropTraverser {
        override def applySymbol(s: Sym) = syms += s
      })(p)
      syms
    }

    trait PropMap {
      def apply(x: Prop): Prop = x match { // TODO: mapConserve
        case And(ops) => And(ops map apply)
        case Or(ops) => Or(ops map apply)
        case Not(a) => Not(apply(a))
        case p => p
      }
    }

    // to govern how much time we spend analyzing matches for unreachability/exhaustivity
    object AnalysisBudget {
      val maxDPLLdepth = global.settings.YpatmatExhaustdepth.value
      val maxFormulaSize = 100 * math.min(Int.MaxValue / 100, maxDPLLdepth)

      private def advice =
        s"Please try with scalac -Ypatmat-exhaust-depth ${maxDPLLdepth * 2} or -Ypatmat-exhaust-depth off."

      def recursionDepthReached =
        s"Exhaustivity analysis reached max recursion depth, not all missing cases are reported.\n($advice)"

      abstract class Exception(val advice: String) extends RuntimeException("CNF budget exceeded")

      object formulaSizeExceeded extends Exception(s"The analysis required more space than allowed.\n$advice")

    }

    if (System.getProperty("scalac.patmat.analysisBudget") != null) {
      reportWarning(s"Please remove -Dscalac.patmat.analysisBudget, it is ignored.")
    }

    // convert finite domain propositional logic with subtyping to pure boolean propositional logic
    // a type test or a value equality test are modelled as a variable being equal to some constant
    // a variable V may be assigned multiple constants, as long as they do not contradict each other
    // according to subtyping, e.g., V = ConstantType(1) and V = Int are valid assignments
    // we rewrite V = C to a fresh boolean symbol, and model what we know about the variable's domain
    // in a prelude (the equality axioms)
    //   1. a variable with a closed domain (of a sealed type) must be assigned one of the instantiable types in its domain
    //   2. for each variable V in props, and each constant C it is compared to,
    //      compute which assignments imply each other (as in the example above: V = 1 implies V = Int)
    //      and which assignments are mutually exclusive (V = String implies -(V = Int))
    //
    // note that this is a conservative approximation: V = Constant(A) and V = Constant(B)
    // are considered mutually exclusive (and thus both cases are considered reachable in {case A => case B =>}),
    // even though A may be equal to B   (and thus the second case is not "dynamically reachable")
    //
    // TODO: for V1 representing x1 and V2 standing for x1.head, encode that
    //       V1 = Nil implies -(V2 = Ci) for all Ci in V2's domain (i.e., it is unassignable)
    // may throw an AnalysisBudget.Exception
    def removeVarEq(props: List[Prop], modelNull: Boolean = false): (Prop, List[Prop]) = {
      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.patmatAnaVarEq) else null

      val vars = new LogicLinkedHashSet[Var]

      object gatherEqualities extends PropTraverser {
        override def apply(p: Prop) = p match {
          case Eq(v, NullConst) if !modelNull => vars += v // not modeling equality to null
          case Eq(v, c)                       => vars += v; v.registerEquality(c)
          case _                              => super.apply(p)
        }
      }

      object rewriteEqualsToProp extends PropMap {
        override def apply(p: Prop) = p match {
          case Eq(v, c) => v.propForEqualsTo(c)
          case _ => super.apply(p)
        }
      }

      props foreach gatherEqualities.apply
      if (modelNull) vars foreach (_.registerNull())

      val pure = props map (p => rewriteEqualsToProp(p))

      val eqAxioms = ArraySeq.newBuilder[Prop]
      @inline def addAxiom(p: Prop) = eqAxioms += p

      debug.patmat("removeVarEq vars: "+ vars)
      vars.foreach { v =>
        val isScrutineeVar = v == vars.head

        // if v.domainSyms.isEmpty, we must consider the domain to be infinite
        // otherwise, since the domain fully partitions the type of the value,
        // exactly one of the types (and whatever it implies, imposed separately) must be chosen
        // consider X ::= A | B | C, and A => B
        // coverage is formulated as: A \/ B \/ C and the implications are
        v.domainSyms foreach { dsyms =>
          // if the domain is knonw to be empty
          // only add that axiom if the var is the scrutinee
          // which has the effect of wiping out the whole formula
          // (because `\/(Set.empty) == False`)
          // otherwise it's just a subvariable of a match expression
          // which has no domain (i.e. an unreachable branch)
          // but it shouldn't wipe out the whole exhaustivity check
          // neg/t8511 vs (pos/t6146 or neg/virtpatmat_exhaust_compound.scala)
          if (isScrutineeVar || dsyms.nonEmpty)
            addAxiom(\/(dsyms))
        }

        // when this variable cannot be null the equality corresponding to the type test `(x: T)`, where T is x's static type,
        // is always true; when the variable may be null we use the implication `(x != null) => (x: T)` for the axiom
        v.symForStaticTp foreach { symForStaticTp =>
          if (v.mayBeNull) addAxiom(Or(v.propForEqualsTo(NullConst), symForStaticTp))
          else addAxiom(symForStaticTp)
        }

        v.implications foreach { case (sym, implied, excluded) =>
          // when sym is true, what must hold...
          implied  foreach (impliedSym  => addAxiom(Or(Not(sym), impliedSym)))
          // ... and what must not?
          excluded foreach {
            excludedSym =>
              val exclusive = v.groupedDomains.exists {
                domain => domain.contains(sym) && domain.contains(excludedSym)
              }

              // TODO: populate `v.exclusiveDomains` with `Set`s from the start, and optimize to:
              // val exclusive = v.exclusiveDomains.exists { inDomain => inDomain(sym) && inDomain(excludedSym) }
              if (!exclusive)
                addAxiom(Or(Not(sym), Not(excludedSym)))
          }
        }

        // all symbols in a domain are mutually exclusive
        v.groupedDomains.foreach {
          syms => if (syms.size > 1) addAxiom(AtMostOne(syms.toList))
        }
      }

      val eqAxiomsSeq = eqAxioms.result()
      debug.patmat(s"eqAxioms:\n${eqAxiomsSeq.mkString("\n")}")
      debug.patmat(s"pure:${pure.mkString("\n")}")

      if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.patmatAnaVarEq, start)

      (And(eqAxiomsSeq: _*), pure)
    }

    type Solvable

    def propToSolvable(p: Prop): Solvable = {
      val (eqAxiom, pure :: Nil) = removeVarEq(List(p), modelNull = false): @unchecked
      eqFreePropToSolvable(And(eqAxiom, pure))
    }

    def eqFreePropToSolvable(f: Prop): Solvable

    type Model = Map[Sym, Boolean]
    val EmptyModel: Model
    val NoModel: Model

    final case class Solution(model: Model, unassigned: List[Sym])

    def hasModel(solvable: Solvable): Boolean

    def findAllModelsFor(solvable: Solvable, sym: Symbol = NoSymbol): List[Solution]
  }
}

object Logic {
  import scala.annotation.nowarn
  import scala.collection.mutable.{Growable, GrowableBuilder, SetOps}
  import scala.collection.{IterableFactory, IterableFactoryDefaults, StrictOptimizedIterableOps}

  // Local subclass because we can't override `addAll` in the collections (bin compat), see PR scala/scala#10361
  @nowarn("msg=inheritance from class LinkedHashSet")
  class LogicLinkedHashSet[A] extends mutable.LinkedHashSet[A]
    with SetOps[A, LogicLinkedHashSet, LogicLinkedHashSet[A]]
    with StrictOptimizedIterableOps[A, LogicLinkedHashSet, LogicLinkedHashSet[A]]
    with IterableFactoryDefaults[A, LogicLinkedHashSet] {
    override def iterableFactory: IterableFactory[LogicLinkedHashSet] = LogicLinkedHashSet
    override def addAll(xs: IterableOnce[A]): this.type = {
      sizeHint(xs)
      super.addAll(xs)
    }
  }

  object LogicLinkedHashSet extends IterableFactory[LogicLinkedHashSet] {
    override def from[A](source: IterableOnce[A]): LogicLinkedHashSet[A] = Growable.from(empty[A], source)
    override def empty[A]: LogicLinkedHashSet[A] = new LogicLinkedHashSet[A]
    override def newBuilder[A]: mutable.Builder[A, LogicLinkedHashSet[A]] = new GrowableBuilder(empty[A])
  }
}

trait ScalaLogic extends Interface with Logic with TreeAndTypeAnalysis {
  trait TreesAndTypesDomain extends PropositionalLogic with CheckableTreeAndTypeAnalysis {
    type Type = global.Type
    type Tree = global.Tree
    import global.definitions.ConstantNull

    // resets hash consing -- only supposed to be called by TreeMakersToProps
    def prepareNewAnalysis(): Unit = { Var.resetUniques(); Const.resetUniques() }

    object Var extends VarExtractor {
      private[this] var _nextId = 0
      def nextId = {_nextId += 1; _nextId}

      def resetUniques() = {_nextId = 0; uniques.clear()}
      private val uniques = new mutable.HashMap[Tree, Var]
      def apply(x: Tree): Var = uniques.getOrElseUpdate(x, new Var(x, x.tpe))
      def unapply(v: Var) = Some(v.path)
    }
    class Var(val path: Tree, staticTp: Type) extends AbsVar {
      private[this] val id: Int = Var.nextId

      // private[this] var canModify: Option[Array[StackTraceElement]] = None
      private[this] def ensureCanModify() = {} //if (canModify.nonEmpty) debug.patmat("BUG!"+ this +" modified after having been observed: "+ canModify.get.mkString("\n"))

      private[this] def observed() = {} //canModify = Some(Thread.currentThread.getStackTrace)

      // don't access until all potential equalities have been registered using registerEquality
      private[this] val symForEqualsTo = new mutable.HashMap[Const, Sym]

      // when looking at the domain, we only care about types we can check at run time
      val staticTpCheckable: Type = checkableType(staticTp)

      private[this] var _mayBeNull = false
      def registerNull(): Unit = { ensureCanModify(); if (ConstantNull <:< staticTpCheckable) _mayBeNull = true }
      def mayBeNull: Boolean = _mayBeNull

      // case None => domain is unknown,
      // case Some(List(tps: _*)) => domain is exactly tps
      // we enumerate the subtypes of the full type, as that allows us to filter out more types statically,
      // once we go to run-time checks (on Const's), convert them to checkable types
      // TODO: there seems to be bug for singleton domains (variable does not show up in model)
      lazy val domain: Option[Set[Const]] = {
        val subConsts =
          enumerateSubtypes(staticTp, grouped = false)
          .headOption.map { tps =>
          tps.to(scala.collection.immutable.ListSet).map { tp =>
            val domainC = TypeConst(tp)
            registerEquality(domainC)
            domainC
          }
        }

        val allConsts =
          if (mayBeNull) {
            registerEquality(NullConst)
            subConsts map (_ + NullConst)
          } else
            subConsts

        observed(); allConsts
      }

      lazy val groupedDomains: List[Set[Sym]] = {
        val subtypes = enumerateSubtypes(staticTp, grouped = true)
        subtypes.map {
          subTypes =>
            val syms = subTypes.flatMap(tpe => symForEqualsTo.get(TypeConst(tpe))).to(scala.collection.immutable.ListSet)
            if (mayBeNull) syms + symForEqualsTo(NullConst) else syms
        }.filter(_.nonEmpty)
      }

      // populate equalitySyms
      // don't care about the result, but want only one fresh symbol per distinct constant c
      def registerEquality(c: Const): Unit = { ensureCanModify() ; symForEqualsTo.getOrElseUpdate(c, Sym(this, c)) }

      // return the symbol that represents this variable being equal to the constant `c`, if it exists, otherwise False (for robustness)
      // (registerEquality(c) must have been called prior, either when constructing the domain or from outside)
      def propForEqualsTo(c: Const): Prop = { observed() ; symForEqualsTo.getOrElse(c, False) }

      // [implementation NOTE: don't access until all potential equalities have been registered using registerEquality]p
      /** the information needed to construct the boolean proposition that encodes the equality proposition (V = C)
       *
       * that models a type test pattern `_: C` or constant pattern `C`, where the type test gives rise to a TypeConst C,
       * and the constant pattern yields a ValueConst C
       *
       * for exhaustivity, we really only need implication (e.g., V = 1 implies that V = 1 /\ V = Int, if both tests occur in the match,
       * and thus in this variable's equality symbols), but reachability also requires us to model things like V = 1 precluding V = "1"
       */
      lazy val implications = {
        /* when we know V = C, which other equalities must hold
         *
         * in general, equality to some type implies equality to its supertypes
         * (this multi-valued kind of equality is necessary for unreachability)
         * note that we use subtyping as a model for implication between instanceof tests
         * i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
         * unfortunately this is not true in general (see e.g. scala/bug#6022)
         */
        def implies(lower: Const, upper: Const): Boolean =
          // values and null
            lower == upper ||
          // type implication
            (lower != NullConst && !upper.isValue &&
             instanceOfTpImplies(if (lower.isValue) lower.wideTp else lower.tp, upper.tp))

          // if(r) debug.patmat("implies    : "+(lower, lower.tp, upper, upper.tp))
          // else  debug.patmat("NOT implies: "+(lower, upper))


        /* Does V=A preclude V=B?
         *
         * (0) A or B must be in the domain to draw any conclusions.
         *
         *     For example, knowing the scrutinee is *not* true does not
         *     statically exclude it from being `X`, because that is an opaque
         *     Boolean.
         *
         *     val X = true
         *     (true: Boolean) match { case true => case X <reachable> }
         *
         * (1) V = null excludes assignment to any other constant (modulo point #0). This includes
         *     both values and type tests (which are both modelled here as `Const`)
         * (2) V = A and V = B, for A and B domain constants, are mutually exclusive unless A == B
         *
         * (3) We only reason about test tests as being excluded by null assignments, otherwise we
         *     only consider value assignments.
         *     TODO: refine this, a == 0 excludes a: String, or `a: Int` excludes `a: String`
         *     (since no value can be of both types. See also scala/bug#7211)
         *
         *  NOTE: V = 1 does not preclude V = Int, or V = Any, it could be said to preclude
         *        V = String, but we don't model that.
         */
        def excludes(a: Const, b: Const): Boolean = {
          val bothInDomain  = domain exists (d => d(a) && d(b))
          val eitherIsNull  = a == NullConst || b == NullConst
          val bothAreValues = a.isValue && b.isValue
          bothInDomain && (eitherIsNull || bothAreValues) && (a != b)
        }

          // if(r) debug.patmat("excludes    : "+(a, a.tp, b, b.tp))
          // else  debug.patmat("NOT excludes: "+(a, b))

/*
[ HALF BAKED FANCINESS: //!equalitySyms.exists(common => implies(common.const, a) && implies(common.const, b)))
 when type tests are involved, we reason (conservatively) under a closed world assumption,
 since we are really only trying to counter the effects of the symbols that we introduce to model type tests
 we don't aim to model the whole subtyping hierarchy, simply to encode enough about subtyping to do unreachability properly

 consider the following hierarchy:

    trait A
    trait B
    trait C
    trait AB extends B with A

  // two types are mutually exclusive if there is no equality symbol whose constant implies both
  object Test extends App {
    def foo(x: Any) = x match {
      case _ : C  => println("C")
      case _ : AB => println("AB")
      case _ : (A with B) => println("AB'")
      case _ : B  => println("B")
      case _ : A  => println("A")
    }

 of course this kind of reasoning is not true in general,
 but we can safely pretend types are mutually exclusive as long as there are no counter-examples in the match we're analyzing}
*/

        val excludedPair = new mutable.HashSet[ExcludedPair]

        case class ExcludedPair(a: Const, b: Const) {
          override def equals(o: Any) = o match {
            case ExcludedPair(aa, bb) => (a == aa && b == bb) || (a == bb && b == aa)
            case _ => false
          }
          // make ExcludedPair(a, b).hashCode == ExcludedPair(b, a).hashCode
          override def hashCode = a.hashCode ^ b.hashCode
        }

        equalitySyms map { sym =>
          // if we've already excluded the pair at some point (-A \/ -B), then don't exclude the symmetric one (-B \/ -A)
          // (nor the positive implications -B \/ A, or -A \/ B, which would entail the equality axioms falsifying the whole formula)
          val todo = equalitySyms filterNot (b => (b.const == sym.const) || excludedPair(ExcludedPair(b.const, sym.const)))
          val (excluded, notExcluded) = partitionConserve(todo)(b => excludes(sym.const, b.const))
          val implied = notExcluded filter (b => implies(sym.const, b.const))

          debug.patmat("eq axioms for: "+ sym.const)
          debug.patmat("excluded: "+ excluded)
          debug.patmat("implied: "+ implied)

          excluded foreach { excludedSym => excludedPair += ExcludedPair(sym.const, excludedSym.const)}

          (sym, implied, excluded)
        }
      }

      // accessing after calling registerNull will result in inconsistencies
      lazy val domainSyms: Option[Set[Sym]] = domain map { _ map symForEqualsTo }

      lazy val symForStaticTp: Option[Sym]  = symForEqualsTo.get(TypeConst(staticTpCheckable))

      // don't access until all potential equalities have been registered using registerEquality
      private lazy val equalitySyms = {observed(); symForEqualsTo.values.toList.sortBy(_.toString) }

      // don't call until all equalities have been registered and registerNull has been called (if needed)
      def describe = {
        val consts = symForEqualsTo.keys.toSeq.sortBy(_.toString)
        def domain_s = domain match {
          case Some(d) => d.mkString(" ::= ", " | ", "// " + consts)
          case _       => consts.mkString(" ::= ", " | ", " | ...")
        }
        s"$this: ${staticTp}${domain_s} // = $path"
      }
      override def toString = "V"+ id
    }


    import global.definitions._
    import global.{ConstantType, Ident, Literal, NoSymbol, SingletonType, TypeBounds, singleType}


    // all our variables range over types
    // a literal constant becomes ConstantType(Constant(v)) when the type allows it (roughly, anyval + string + null)
    // equality between variables: SingleType(x) (note that pattern variables cannot relate to each other -- it's always patternVar == nonPatternVar)
    object Const {
      def resetUniques() = {_nextTypeId = 0; _nextValueId = 0; uniques.clear() ; trees.clear()}

      private[this] var _nextTypeId = 0
      def nextTypeId = {_nextTypeId += 1; _nextTypeId}

      private[this] var _nextValueId = 0
      def nextValueId = {_nextValueId += 1; _nextValueId}

      private val uniques = new mutable.HashMap[Type, Const]
      private[TreesAndTypesDomain] def unique(tp: Type, mkFresh: => Const): Const =
        uniques.getOrElse(tp, {
          // normalize to increase the chance of structural equality and reduce the cost of =:=
          val normalized = tp.map(_.normalize)
          uniques.getOrElseUpdate(normalized, {
            val const = uniques.keysIterator.find(_ =:= normalized).fold(mkFresh)(uniques)
            debug.patmat(s"unique const: ${tp -> const}")
            const
          })
        })

      private val trees = mutable.HashSet.empty[Tree]

      // hashconsing trees (modulo value-equality)
      private[TreesAndTypesDomain] def uniqueTpForTree(t: Tree): Type = {
        def freshExistentialSubtype(tp: Type): Type = {
          // scala/bug#8611 tp.narrow is tempting, but unsuitable. See `testRefinedTypeSI8611` for an explanation.
          NoSymbol.freshExistential("", 0).setInfo(TypeBounds.upper(tp)).tpe
        }

        if (!t.symbol.isStable) {
          // Create a fresh type for each unstable value, since we can never correlate it to another value.
          // For example `case X => case X =>` should not complain about the second case being unreachable,
          // if X is mutable.
          freshExistentialSubtype(t.tpe)
        }
        else trees find (a => equivalentTree(a, t)) match {
          case Some(orig) =>
            debug.patmat("unique tp for tree: " + ((orig, orig.tpe)))
            orig.tpe
          case _ =>
            // duplicate, don't mutate old tree (TODO: use a map tree -> type instead?)
            val treeWithNarrowedType = t.duplicate setType freshExistentialSubtype(t.tpe)
            debug.patmat("uniqued: "+ ((t, t.tpe, treeWithNarrowedType.tpe)))
            trees += treeWithNarrowedType
            treeWithNarrowedType.tpe
        }
      }
    }

    sealed abstract class Const {
      def tp: Type
      def wideTp: Type

      def isAny = wideTp =:= AnyTpe
      def isValue: Boolean //= tp.isStable

      // note: use reference equality on Const since they're hash-consed (doing type equality all the time is too expensive)
      // the equals inherited from AnyRef does just this
    }

    // find most precise super-type of tp that is a class
    // we skip non-class types (singleton types, abstract types) so that we can
    // correctly compute how types relate in terms of the values they rule out
    // e.g., when we know some value must be of type T, can it still be of type S? (this is the positive formulation of what `excludes` on Const computes)
    // since we're talking values, there must have been a class involved in creating it, so rephrase our types in terms of classes
    // (At least conceptually: `true` is an instance of class `Boolean`)
    private def widenToClass(tp: Type): Type =
      if (tp.typeSymbol.isClass) tp
      else if (tp.baseClasses.isEmpty) AnyTpe
      else tp.baseType(tp.baseClasses.head)

    object TypeConst extends TypeConstExtractor {
      def apply(tp: Type) = {
        if (tp =:= ConstantNull) NullConst
        else if (tp.isInstanceOf[SingletonType]) ValueConst.fromType(tp)
        else Const.unique(tp, new TypeConst(tp))
      }
      def unapply(c: TypeConst): Some[Type] = Some(c.tp)
    }

    // corresponds to a type test that does not imply any value-equality (well, except for outer checks, which we don't model yet)
    sealed class TypeConst(val tp: Type) extends Const {
      assert(!(tp =:= ConstantNull))
      /*private[this] val id: Int = */ Const.nextTypeId

      val wideTp = widenToClass(tp)
      def isValue = false
      override def toString = tp.toString //+"#"+ id
    }

    // p is a unique type or a constant value
    object ValueConst extends ValueConstExtractor {
      def fromType(tp: Type) = {
        assert(tp.isInstanceOf[SingletonType])
        val toString = tp match {
          case ConstantType(c) => c.escapedStringValue
          case _ if tp.typeSymbol.isModuleClass => tp.typeSymbol.name.toString
          case _ => tp.toString
        }
        Const.unique(tp, new ValueConst(tp, tp.widen, toString))
      }
      def apply(p: Tree) = {
        val tp = p.tpe.normalize
        if (tp =:= ConstantNull) NullConst
        else {
          val wideTp = widenToClass(tp)

          val narrowTp =
            if (tp.isInstanceOf[SingletonType]) tp
            else p match {
              case Literal(c) =>
                if (c.tpe =:= UnitTpe) c.tpe
                else ConstantType(c)
              case Ident(_) if p.symbol.isStable =>
                // for Idents, can encode uniqueness of symbol as uniqueness of the corresponding singleton type
                // for Selects, which are handled by the next case, the prefix of the select varies independently of the symbol (see neg/virtpatmat_unreach_select.scala)
                singleType(tp.prefix, p.symbol)
              case _ =>
                Const.uniqueTpForTree(p)
            }

          val toString =
            if (p.hasSymbolField && p.symbol.isStable) p.symbol.name.toString // tp.toString
            else p.toString //+"#"+ id

          Const.unique(narrowTp, new ValueConst(narrowTp, checkableType(wideTp), toString)) // must make wide type checkable so that it is comparable to types from TypeConst
        }
      }
    }
    sealed class ValueConst(val tp: Type, val wideTp: Type, override val toString: String) extends Const {
      // debug.patmat("VC"+(tp, wideTp, toString))
      assert(!(tp =:= ConstantNull)) // TODO: assert(!tp.isStable)
      /*private[this] val id: Int = */Const.nextValueId
      def isValue = true
    }

    case object NullConst extends Const {
      def tp     = ConstantNull
      def wideTp = ConstantNull

      def isValue = true
      override def toString = "null"
    }
  }
}
