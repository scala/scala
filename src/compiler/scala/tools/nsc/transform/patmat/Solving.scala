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

package scala.tools.nsc.transform.patmat

import scala.annotation.{tailrec, unused}
import scala.collection.{immutable, mutable}, mutable.ArrayBuffer

/** Solve pattern matcher exhaustivity problem via DPLL. */
trait Solving extends Logic {
  import global._

  trait CNF extends PropositionalLogic {
    // a literal is a (possibly negated) variable
    type Lit <: LitApi
    trait LitApi {
      def unary_- : Lit
    }

    def Lit: LitModule
    trait LitModule {
      def apply(v: Int): Lit
    }

    type Clause = Set[Lit]

    val NoClauses: Array[Clause]    = Array()
    val ArrayOfFalse: Array[Clause] = Array(clause())

    // a clause is a disjunction of distinct literals
    def clause(): Clause                          = Set.empty
    def clause(l: Lit): Clause                    = Set.empty + l
    def clause(l: Lit, l2: Lit): Clause           = Set.empty + l + l2
    def clause(l: Lit, l2: Lit, ls: Lit*): Clause = Set.empty + l + l2 ++ ls
    def clause(ls: IterableOnce[Lit]): Clause     = Set.from(ls)

    /** Conjunctive normal form (of a Boolean formula).
     *  A formula in this form is amenable to a SAT solver
     *  (i.e., solver that decides satisfiability of a formula).
     */
    type Cnf = Array[Clause]

    class SymbolMapping(symbols: collection.Set[Sym]) {
      val variableForSymbol: Map[Sym, Int] = {
        symbols.iterator.zipWithIndex.map {
          case (sym, i) => sym -> (i + 1)
        }.toMap
      }

      val symForVar: Map[Int, Sym] = variableForSymbol.map(_.swap)

      val relevantVars = symForVar.keysIterator.map(math.abs).to(immutable.BitSet)

      def lit(sym: Sym): Lit = Lit(variableForSymbol(sym))

      def size = symbols.size
    }

    def cnfString(f: Array[Clause]): String

    final case class Solvable(cnf: Cnf, symbolMapping: SymbolMapping) {
      def ++(other: Solvable) = {
        require(this.symbolMapping eq other.symbolMapping,
          "this and other must have the same symbol mapping (same reference)")
        Solvable(cnf ++ other.cnf, symbolMapping)
      }

      override def toString: String = {
        "Solvable\nLiterals:\n" +
          (for {
            (lit, sym) <- symbolMapping.symForVar.toSeq.sortBy(_._1)
          } yield {
            s"$lit -> $sym"
          }).mkString("\n") + "Cnf:\n" + cnfString(cnf)
      }
    }

    trait CnfBuilder {
      private[this] val buff = ArrayBuffer[Clause]()

      var literalCount: Int

      /**
       * @return new Tseitin variable
       */
      def newLiteral(): Lit = {
        literalCount += 1
        Lit(literalCount)
      }

      lazy val constTrue: Lit = {
        val constTrue = newLiteral()
        addClauseProcessed(clause(constTrue))
        constTrue
      }

      def constFalse: Lit = -constTrue

      def isConst(l: Lit): Boolean = l == constTrue || l == constFalse

      def addClauseProcessed(clause: Clause): Unit = {
        if (clause.nonEmpty) {
          buff += clause
        }
      }

      def buildCnf: Array[Clause] = {
        val cnf = buff.toArray
        buff.clear()
        cnf
      }

    }

    /** Plaisted transformation: used for conversion of a
      * propositional formula into conjunctive normal form (CNF)
      * (input format for SAT solver).
      * A simple conversion into CNF via Shannon expansion would
      * also be possible but it's worst-case complexity is exponential
      * (in the number of variables) and thus even simple problems
      * could become untractable.
      * The Plaisted transformation results in an _equisatisfiable_
      * CNF-formula (it generates auxiliary variables)
      * but runs with linear complexity.
      * The common known Tseitin transformation uses bi-implication,
      * whereas the Plaisted transformation uses implication only, thus
      * the resulting CNF formula has (on average) only half of the clauses
      * of a Tseitin transformation.
      * The Plaisted transformation uses the polarities of sub-expressions
      * to figure out which part of the bi-implication can be omitted.
      * However, if all sub-expressions have positive polarity
      * (e.g., after transformation into negation normal form)
      * then the conversion is rather simple and the pseudo-normalization
      * via NNF increases chances only one side of the bi-implication
      * is needed.
      */
    class TransformToCnf(symbolMapping: SymbolMapping) extends CnfBuilder {

      // new literals start after formula symbols
      var literalCount: Int = symbolMapping.size

      def convertSym(sym: Sym): Lit = symbolMapping.lit(sym)

      def apply(p: Prop): Solvable = {

        def convert(p: Prop): Option[Lit] = {
          p match {
            case And(fv)        => Some(and(fv.flatMap(convert)))
            case Or(fv)         => Some(or(fv.flatMap(convert)))
            case Not(a)         => convert(a).map(not)
            case sym: Sym       => Some(convertSym(sym))
            case True           => Some(constTrue)
            case False          => Some(constFalse)
            case AtMostOne(ops) => atMostOne(ops) ; None
            case _: Eq          => throw new MatchError(p)
          }
        }

        def and(bv: collection.Set[Lit]): Lit = {
          if (bv.isEmpty) {
            // this case can actually happen because `removeVarEq` could add no constraints
            constTrue
          } else if (bv.size == 1) {
            bv.head
          } else if (bv.contains(constFalse)) {
            constFalse
          } else {
            // op1 /\ op2 /\ ... /\ opx <==>
            // (o -> op1) /\ (o -> op2) ... (o -> opx) /\ (!op1 \/ !op2 \/... \/ !opx \/ o)
            // (!o \/ op1) /\ (!o \/ op2) ... (!o \/ opx) /\ (!op1 \/ !op2 \/... \/ !opx \/ o)
            val new_bv = bv.toSet - constTrue // ignore `True`
            val o = newLiteral() // auxiliary Tseitin variable
            new_bv.foreach(op => addClauseProcessed(clause(op, -o)))
            o
          }
        }

        def or(bv: collection.Set[Lit]): Lit = {
          if (bv.isEmpty) {
            constFalse
          } else if (bv.size == 1) {
            bv.head
          } else if (bv.contains(constTrue)) {
            constTrue
          } else {
            // op1 \/ op2 \/ ... \/ opx <==>
            // (op1 -> o) /\ (op2 -> o) ... (opx -> o) /\ (op1 \/ op2 \/... \/ opx \/ !o)
            // (!op1 \/ o) /\ (!op2 \/ o) ... (!opx \/ o) /\ (op1 \/ op2 \/... \/ opx \/ !o)
            val new_bv = bv.toSet - constFalse // ignore `False`
            val o = newLiteral() // auxiliary Tseitin variable
            addClauseProcessed(new_bv + (-o))
            o
          }
        }

        // no need for auxiliary variable
        def not(a: Lit): Lit = -a

        /*
         * This encoding adds 3n-4 variables auxiliary variables
         * to encode that at most 1 symbol can be set.
         * See also "Towards an Optimal CNF Encoding of Boolean Cardinality Constraints"
         * http://www.carstensinz.de/papers/CP-2005.pdf
         */
        def atMostOne(ops: List[Sym]): Unit = {
          (ops: @unchecked) match {
            case hd :: Nil  => convertSym(hd)
            case x1 :: tail =>
              // sequential counter: 3n-4 clauses
              // pairwise encoding: n*(n-1)/2 clauses
              // thus pays off only if n > 5
              if (ops.lengthCompare(5) > 0) {

                @inline
                def /\(a: Lit, b: Lit) = addClauseProcessed(clause(a, b))

                val (mid, xn :: Nil) = tail.splitAt(tail.size - 1): @unchecked

                // 1 <= x1,...,xn <==>
                //
                // (!x1 \/ s1) /\ (!xn \/ !sn-1) /\
                //
                //     /\
                //    /  \ (!xi \/ si) /\ (!si-1 \/ si) /\ (!xi \/ !si-1)
                //  1 < i < n
                val s1 = newLiteral()
                /\(-convertSym(x1), s1)
                val snMinus = mid.foldLeft(s1) {
                  case (siMinus, sym) =>
                    val xi = convertSym(sym)
                    val si = newLiteral()
                    /\(-xi, si)
                    /\(-siMinus, si)
                    /\(-xi, -siMinus)
                    si
                }
                /\(-convertSym(xn), -snMinus)
              } else {
                ops.map(convertSym).combinations(2).foreach {
                  case a :: b :: Nil =>
                    addClauseProcessed(clause(-a, -b))
                  case _             =>
                }
              }
          }
        }

        // add intermediate variable since we want the formula to be SAT!
        addClauseProcessed(convert(p).toSet)

        Solvable(buildCnf, symbolMapping)
      }
    }

    class AlreadyInCNF(symbolMapping: SymbolMapping) {

      object ToLiteral {
        def unapply(f: Prop): Option[Lit] = f match {
          case Not(ToLiteral(lit)) => Some(-lit)
          case sym: Sym            => Some(symbolMapping.lit(sym))
          case _                   => None
        }
      }

      object ToDisjunction {
        def unapply(f: Prop): Option[Array[Clause]] = f match {
          case Or(fv)         =>
            val cl = fv.foldLeft(Option(clause())) {
              case (Some(clause), ToLiteral(lit)) =>
                Some(clause + lit)
              case (_, _)                         =>
                None
            }
            cl.map(Array(_))
          case True           => Some(NoClauses) // empty, no clauses needed
          case False          => Some(ArrayOfFalse) // empty clause can't be satisfied
          case ToLiteral(lit) => Some(Array(clause(lit)))
          case _              => None
        }
      }

      /**
       * Checks if propositional formula is already in CNF
       */
      object ToCnf {
        def unapply(f: Prop): Option[Solvable] = f match {
          case ToDisjunction(clauses) => Some(Solvable(clauses, symbolMapping) )
          case And(fv)                =>
            val clauses = fv.foldLeft(Option(mutable.ArrayBuffer[Clause]())) {
              case (Some(cnf), ToDisjunction(clauses)) =>
                Some(cnf ++= clauses)
              case (_, _)                              =>
                None
            }
            clauses.map(c => Solvable(c.toArray, symbolMapping))
          case _                      => None
        }
      }
    }

    def eqFreePropToSolvable(p: Prop): Solvable = {
      val simplified = simplify(p)
      if (doesFormulaExceedSize(simplified)) {
        throw AnalysisBudget.formulaSizeExceeded
      }

      // collect all variables since after simplification / CNF conversion
      // they could have been removed from the formula
      val symbolMapping = new SymbolMapping(gatherSymbols(p))
      val cnfExtractor = new AlreadyInCNF(symbolMapping)
      val cnfTransformer = new TransformToCnf(symbolMapping)

      def cnfFor(prop: Prop): Solvable =
        prop match {
          // this is needed because t6942 would generate too many clauses with Tseitin
          // already in CNF, just add clauses
          case cnfExtractor.ToCnf(solvable) => solvable
          case prop                         => cnfTransformer.apply(prop)
        }

      simplified match {
        case And(props) =>
          // scala/bug#6942:
          // CNF(P1 /\ ... /\ PN) == CNF(P1) ++ CNF(...) ++ CNF(PN)
          val cnfs = new Array[Solvable](props.size)
          @unused val copied = props.iterator.map(x => cnfFor(x)).copyToArray(cnfs)
          //assert(copied == cnfs.length, "")
          new Solvable(cnfs.flatten[Clause](_.cnf, reflect.classTag[Clause]), cnfs.head.symbolMapping)
        case simplified => cnfFor(simplified)
      }
    }

    /** Using the same identity as `eqFreePropToSolvable`, CNF(P1 /\ ... /\ PN) == CNF(P1) ++ ... ++ CNF(PN),
     *  provided that the conversions share the symbol mapping and the numbering of Tseitin variables.
     *  Unlike `eqFreePropToSolvable(And(prefix :+ p))`, this does not simplify across conjuncts, but the
     *  result is equisatisfiable. Duplicate conjuncts are dropped and counted once against the budget,
     *  as `simplify` would.
     */
    def incrementalSolver(allProps: Iterable[Prop]): IncrementalSolver = new IncrementalSolver {
      private val symbolMapping = new SymbolMapping(gatherSymbols(And.create(allProps)))
      private val cnfExtractor = new AlreadyInCNF(symbolMapping)
      private val cnfTransformer = new TransformToCnf(symbolMapping)
      // only used for membership; clauses are added in the order of the (linked) conjuncts
      private val prefixConjuncts = mutable.HashSet[Prop]()
      private val prefix = ArrayBuffer[Clause]()

      // the unit clause for `constTrue` is emitted on first use, but every query must include it
      cnfTransformer.constTrue
      prefix ++= cnfTransformer.buildCnf

      private def conjuncts(p: Prop): Iterable[Prop] = simplify(p) match {
        case And(ps) => ps
        case True    => Nil
        case p       => p :: Nil
      }
      private def cnfFor(p: Prop): Array[Clause] = {
        if (doesFormulaExceedSize(p)) throw AnalysisBudget.formulaSizeExceeded
        p match {
          case cnfExtractor.ToCnf(solvable) => solvable.cnf
          case p                            => cnfTransformer.apply(p).cnf
        }
      }
      private def checkSize(n: Int): Unit =
        if (n > AnalysisBudget.maxFormulaSize) throw AnalysisBudget.formulaSizeExceeded

      def addToPrefix(p: Prop): Unit =
        conjuncts(p).foreach { c =>
          if (prefixConjuncts.add(c)) {
            checkSize(prefixConjuncts.size)
            prefix ++= cnfFor(c)
          }
        }

      def hasModelWith(p: Prop): Boolean = {
        val extra = conjuncts(p).iterator.filterNot(prefixConjuncts).toList.distinct
        checkSize(prefixConjuncts.size + extra.size)
        val clauses = ArrayBuffer.from(prefix)
        extra.foreach(c => clauses ++= cnfFor(c))
        hasModel(Solvable(clauses.toArray, symbolMapping))
      }
    }

    private def doesFormulaExceedSize(p: Prop): Boolean = {
      p match {
        case And(ops) =>
          if (ops.size > AnalysisBudget.maxFormulaSize) {
            true
          } else {
            ops.exists(doesFormulaExceedSize)
          }
        case Or(ops)  =>
          if (ops.size > AnalysisBudget.maxFormulaSize) {
            true
          } else {
            ops.exists(doesFormulaExceedSize)
          }
        case Not(a)   => doesFormulaExceedSize(a)
        case _        => false
      }
    }
  }

  // simple solver using DPLL
  // adapted from https://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)
  trait Solver extends CNF {
    case class Lit(v: Int) extends LitApi {
      private lazy val negated: Lit = Lit(-v)

      def unary_- : Lit     = negated
      def variable: Int     = Math.abs(v)
      def positive: Boolean = v >= 0

      override def toString = s"Lit#$v"
      override def hashCode = v
    }

    object Lit extends LitModule {
      def apply(v: Int): Lit = new Lit(v)
    }

    def cnfString(f: Array[Clause]): String = {
      val lits: Array[List[String]] = f map (_.map(_.toString).toList)
      val xss: List[List[String]] = lits.toList
      val aligned: String = alignAcrossRows(xss, "\\/", " /\\\n")
      aligned
    }

    // empty set of clauses is trivially satisfied
    val EmptyModel = Map.empty[Sym, Boolean]

    // no model: originates from the encounter of an empty clause, i.e.,
    // happens if all variables have been assigned in a way that makes the corresponding literals false
    // thus there is no possibility to satisfy that clause, so the whole formula is UNSAT
    val NoModel: Model = null

    // this model contains the auxiliary variables as well
    type TseitinModel = List[Lit]
    val NoTseitinModel: TseitinModel = null

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(solvable: Solvable, owner: Symbol): List[Solution] = {
      import solvable.{ cnf, symbolMapping }, symbolMapping.{ symForVar, relevantVars }
      debug.patmat(s"find all models for\n${cnfString(cnf)}")

      // we must take all vars from non simplified formula
      // otherwise if we get `T` as formula, we don't expand the variables
      // that are not in the formula...

      // debug.patmat("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      // (i.e. the blocking clause - used for ALL-SAT)
      def negateModel(m: TseitinModel): TseitinModel = {
        // filter out auxiliary Tseitin variables
        m.filter(lit => relevantVars.contains(lit.variable)).map(lit => -lit)
      }

      def newSolution(model: TseitinModel, unassigned: List[Int]): Solution = {
        val newModel: Model = if (model eq NoTseitinModel) NoModel else {
          model.iterator.collect {
            case lit if symForVar.isDefinedAt(lit.variable) => (symForVar(lit.variable), lit.positive)
          }.to(scala.collection.immutable.ListMap)
        }
        Solution(newModel, unassigned.map(symForVar))
      }

      @tailrec
      def findAllModels(clauses: Array[Clause],
                        models: List[Solution],
                        recursionDepthAllowed: Int = AnalysisBudget.maxDPLLdepth): List[Solution] = {
        if (recursionDepthAllowed == 0) {
          uncheckedWarning(owner.pos, AnalysisBudget.recursionDepthReached, owner)
          models
        } else {
          debug.patmat(s"find all models for\n${cnfString(clauses)}")
          val model = findTseitinModelFor(clauses)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model eq NoTseitinModel) models else {
            // note that we should not expand the auxiliary variables (from Tseitin transformation)
            // since they are existentially quantified in the final solution
            val unassigned: List[Int] = relevantVars.filterNot(x => model.exists(lit => x == lit.variable)).toList.sorted
            debug.patmat(s"unassigned $unassigned in $model")

            val solution = newSolution(model, unassigned)
            val negated  = negateModel(model).to(scala.collection.immutable.ListSet)
            findAllModels(clauses :+ negated, solution :: models, recursionDepthAllowed - 1)
          }
        }
      }

      findAllModels(solvable.cnf, Nil)
    }

    def hasModel(solvable: Solvable): Boolean = findTseitinModelFor(solvable.cnf) != NoTseitinModel

    def findTseitinModelFor(clauses: Array[Clause]): TseitinModel = {
      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.patmatAnaDPLL) else null

      debug.patmat(s"DPLL\n${cnfString(clauses)}")
      val satisfiableWithModel = findTseitinModel0((java.util.Arrays.copyOf(clauses, clauses.length), Nil) :: Nil)

      if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.patmatAnaDPLL, start)
      satisfiableWithModel
    }

    type TseitinSearch = List[(Array[Clause], List[Lit])]

    /** An implementation of the DPLL algorithm for checking satisfiability
      * of a Boolean formula in CNF (conjunctive normal form).
      *
      * This is a backtracking, depth-first algorithm, which searches a
      * (conceptual) decision tree the nodes of which represent assignments
      * of truth values to variables. The algorithm works like so:
      *
      * - If there are any empty clauses, the formula is unsatisfiable.
      * - If there are no clauses, the formula is trivially satisfiable.
      * - If there is a clause with a single positive (rsp. negated) variable
      *   in it, any solution must assign it the value `true` (rsp. `false`).
      *   Therefore, assign it that value, and perform Boolean Constraint
      *   Propagation on the remaining clauses:
      *   - Any disjunction containing the variable in a positive (rsp. negative)
      *     usage is trivially true, and can be dropped.
      *   - Any disjunction containing the variable in a negative (rsp. positive)
      *     context will not be satisfied using that variable, so it can be
      *     removed from the disjunction.
      * - Otherwise, pick a variable:
      *   - If it always (rsp. never) appears negated (a pure variable), then
      *     any solution must assign the value `true` to it (rsp. `false`)
      *   - Otherwise, try to solve the formula assuming that the variable is
      *     `true`; if no model is found, try to solve assuming it is `false`.
      *
      * See also [[https://en.wikipedia.org/wiki/DPLL_algorithm]].
      *
      * This implementation uses a `List` to reify the search stack, thus making
      * it run in constant stack space. The stack is composed of pairs of
      * `(remaining clauses, variable assignments)`, and depth-first search
      * is achieved by using a stack rather than a queue.
      *
      * Boolean Constraint Propagation runs to a fixpoint in time linear in the size
      * of the formula, using occurrence lists. (Rescanning all clauses for each unit
      * literal made a query quadratic, and the reachability analysis of an N-case
      * match cubic, in N.) Unit clauses are still propagated in the order of their
      * position in the clause array, which is the order in which a rescan would find
      * them, since dropping satisfied clauses and false literals preserves that order.
      * So the models, and thus the counter-examples, are as before.
      */
    private def findTseitinModel0(state: TseitinSearch): TseitinModel = {
      val pos = new java.util.BitSet()
      val neg = new java.util.BitSet()

      // returns the clauses remaining after unit propagation and the extended assignments, or null on conflict
      def propagate(clauses: Array[Clause], assignments0: List[Lit]): (Array[Clause], List[Lit]) = {
        var n = 0
        while (n < clauses.length && clauses(n) != null) n += 1
        val units = new java.util.BitSet()
        var i = 0
        var conflict = false
        var maxVar = 0
        while (i < n) {
          val c = clauses(i)
          c.size match {
            case 0 => conflict = true
            case 1 => units.set(i)
            case _ =>
          }
          c.foreach(l => if (l.variable > maxVar) maxVar = l.variable)
          i += 1
        }
        if (conflict) return null
        if (units.isEmpty) return (clauses, assignments0)

        // occurrence lists, indexed by 2 * variable + (if positive 0 else 1)
        val occStart = new Array[Int](2 * maxVar + 3)
        i = 0
        while (i < n) { clauses(i).foreach(l => occStart(2 * l.variable + (if (l.positive) 0 else 1) + 1) += 1); i += 1 }
        var k = 1
        while (k < occStart.length) { occStart(k) += occStart(k - 1); k += 1 }
        val occ = new Array[Int](occStart(occStart.length - 1))
        val fill = java.util.Arrays.copyOf(occStart, occStart.length)
        i = 0
        while (i < n) { val ci = i; clauses(i).foreach { l => val key = 2 * l.variable + (if (l.positive) 0 else 1); occ(fill(key)) = ci; fill(key) += 1 }; i += 1 }

        val size = new Array[Int](n)
        val sat = new Array[Boolean](n)
        val value = new Array[Byte](maxVar + 1) // 0: unassigned, 1: true, -1: false
        i = 0
        while (i < n) { size(i) = clauses(i).size; i += 1 }

        var assignments = assignments0
        var u = units.nextSetBit(0)
        while (u >= 0) {
          units.clear(u)
          if (!sat(u)) {
            val unitLit = clauses(u).find(l => value(l.variable) == 0).get
            assignments = unitLit :: assignments
            value(unitLit.variable) = if (unitLit.positive) 1 else -1
            val key = 2 * unitLit.variable + (if (unitLit.positive) 0 else 1)
            var j = occStart(key)
            while (j < occStart(key + 1)) { sat(occ(j)) = true; j += 1 }
            val negKey = key ^ 1
            j = occStart(negKey)
            while (j < occStart(negKey + 1)) {
              val c = occ(j)
              if (!sat(c)) {
                size(c) -= 1
                if (size(c) == 0) return null
                if (size(c) == 1) units.set(c)
              }
              j += 1
            }
          }
          u = units.nextSetBit(0)
        }

        val remaining = new ArrayBuffer[Clause](n)
        i = 0
        while (i < n) {
          if (!sat(i)) {
            val c = clauses(i)
            remaining += (if (size(i) == c.size) c else c.filter(l => value(l.variable) == 0))
          }
          i += 1
        }
        (remaining.toArray, assignments)
      }

      @tailrec def loop(state: TseitinSearch): TseitinModel = state match {
        case Nil => NoTseitinModel
        case (clauses0, assignments0) :: rest =>
          propagate(clauses0, assignments0) match {
            case null => loop(rest)
            case (clauses, assignments) =>
              if (clauses.isEmpty || clauses.head == null) assignments
              else {
                // partition symbols according to whether they appear in positive and/or negative literals
                pos.clear()
                neg.clear()
                for (clause <- clauses) {
                  if (clause != null) {
                    clause.foreach { lit: Lit =>
                      if (lit.positive) pos.set(lit.variable) else neg.set(lit.variable)
                    }
                  }
                }

                // appearing only in either positive/negative positions

                pos.xor(neg)
                val pures = pos

                if (!pures.isEmpty) {
                  val pureVar = pures.nextSetBit(0)
                  // turn it back into a literal
                  // (since equality on literals is in terms of equality
                  //  of the underlying symbol and its positivity, simply construct a new Lit)
                  val pureLit: Lit = Lit(if (neg.get(pureVar)) -pureVar else pureVar)
                  val simplified = clauses.filterNot(clause => clause != null && clause.contains(pureLit))
                  loop((simplified, pureLit :: assignments) :: rest)
                } else {
                  val split = clauses.find(_ != null).get.head
                  var i = 0
                  var nullIndex = -1
                  while (i < clauses.length && nullIndex == -1) {
                    if (clauses(i) eq null) nullIndex = i
                    i += 1
                  }

                  val effectiveLength = if (nullIndex == -1) clauses.length else nullIndex
                  val posClauses = java.util.Arrays.copyOf(clauses, effectiveLength + 1)
                  val negClauses = java.util.Arrays.copyOf(clauses, effectiveLength + 1)
                  posClauses(effectiveLength) = Set.empty[Lit] + split
                  negClauses(effectiveLength) = Set.empty[Lit] + (-split)

                  val pos = (posClauses, assignments)
                  val neg = (negClauses, assignments)
                  loop(pos :: neg :: rest)
                }
              }
          }
      }
      loop(state)
    }

  }
}
