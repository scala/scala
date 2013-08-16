/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.reflect.internal.util.Statistics
import scala.language.postfixOps
import scala.collection.{ immutable, mutable }
import scala.reflect.internal.util.Sequenceable

/** Solve pattern matcher exhaustivity problem via DPLL.
 */
trait Solving extends Logic {

  import PatternMatchingStats._

  /** Tseitin transformation: used for conversion of a
   *  propositional formula into conjunctive normal form (CNF)
   *  (input format for SAT solver).
   *  A simple conversion into CNF via Shannon expansion would
   *  also be possible but it's worst-case complexity is exponential
   *  (in the number of variables) and thus even simple problems
   *  could become untractable.
   *  The Tseitin transformation results in an _equisatisfiable_
   *  CNF-formula (it generates auxiliary variables)
   *  but runs with linear complexity.
   */
  trait TseitinCNF extends PropositionalLogic {
    type Clause = CNFBuilder#Clause

    def eqFreePropToSolvable(p: Prop): Solvable = {
      type Cache = Map[Prop, Lit]

      val cache = mutable.Map[Prop, Lit]()

      val cnf = new CNFBuilder

      def convertWithoutCache(p: Prop): Lit = {
        p match {
          case And(fv) =>
            and(fv.map(convertWithCache))
          case Or(fv) =>
            or(fv.map(convertWithCache))
          case Not(a) =>
            not(convertWithCache(a))
          case _: Sym =>
            cnf.newLiteral()
          case True =>
            cnf.constTrue
          case False =>
            cnf.constFalse
          case _: Eq =>
            debug.patmat("Forgot to call propToSolvable()?")
            throw new MatchError(p)
        }
      }

      def convertWithCache(p: Prop): Lit = {
        cache.getOrElse(p, {
          val l = convertWithoutCache(p)
          require(!cache.isDefinedAt(p), "loop in formula?")
          cache += (p -> l)
          l
        })
      }

      def and(bv: Set[Lit]): Lit = {
        import cnf._
        if (bv.isEmpty) {
          constTrue
        } else if (bv.size == 1) {
          bv.head
        } else if (bv.contains(constFalse)) {
          constFalse
        } else {
          // op1*op2*...*opx <==> (op1 + o')(op2 + o')... (opx + o')(op1' + op2' +... + opx' + o)
          val new_bv = bv - constTrue // ignore `True`
          val o = newLiteral() // auxiliary Tseitin variable
          new_bv.map(op => addClauseProcessed(op, -o))
          addClauseProcessed((new_bv.map(op => -op) + o).toSeq: _*)
          o
        }
      }

      def or(bv: Set[Lit]): Lit = {
        import cnf._
        if (bv.isEmpty) {
          constFalse
        } else if (bv.size == 1) {
          bv.head
        } else if (bv.contains(constTrue)) {
          constTrue
        } else {
          // op1+op2+...+opx <==> (op1' + o)(op2' + o)... (opx' + o)(op1 + op2 +... + opx + o')
          val new_bv = bv - constFalse // ignore `False`
          val o = newLiteral() // auxiliary Tseitin variable
          new_bv.map(op => addClauseProcessed(-op, o))
          addClauseProcessed((new_bv + (-o)).toSeq: _*)
          o
        }
      }

      // no need for auxiliary variable
      def not(a: Lit): Lit = -a

      object ToLiteral {
        def unapply(f: Prop): Option[Lit] = f match {
          case Not(a) =>
            ToLiteral.unapply(a).map(lit => -lit)
          case _: Sym =>
            Some(convertWithCache(f)) // go via cache in order to get single literal for variable
          case True =>
            Some(cnf.constTrue)
          case False =>
            Some(cnf.constFalse)
          case _ =>
            None
        }
      }

      object ToDisjunction {
        def unapply(p: Prop): Option[Clause] = p match {
          case Or(fv) =>
            fv.map(ToLiteral.unapply).sequence
          case p =>
            ToLiteral.unapply(p).map(Set(_))
        }
      }

      /** Checks if propositional formula is already in CNF
       */
      object ToCnf {
        def unapply(f: Prop): Option[Seq[Clause]] = f match {
          case And(fv) =>
            fv.map(p => ToDisjunction.unapply(p)).sequence.map(_.toSeq)
          case p =>
            ToDisjunction.unapply(p).map(Seq(_))
        }
      }

      val simplified = simplify(p)
      simplified match {
        case ToCnf(clauses) =>
          // already in CNF, just add clauses
          clauses.foreach(cnf.addClauseRaw)
        case p =>
          // add intermediate variable since we want the formula to be SAT!
          cnf.addClauseProcessed(convertWithCache(p))
      }

      // all variables are guaranteed to be in cache
      // (doesn't mean they will appear in the resulting formula)
      val symForVar: Map[Int, Sym] = cache.collect {
        case (sym: Sym, lit) => lit.variable -> sym
      }(collection.breakOut) // breakOut in order to obtain immutable Map

      Solvable(cnf, symForVar)
    }

  }

  // simple solver using DPLL
  trait Solver extends TseitinCNF {
    /** Override Array creation for efficiency (to not go through reflection). */
    private implicit val clauseTag: scala.reflect.ClassTag[Clause] = new scala.reflect.ClassTag[Clause] {
      def runtimeClass: java.lang.Class[Clause] = classOf[Clause]
      final override def newArray(len: Int): Array[Clause] = new Array[Clause](len)
    }

    def cnfString(f: Array[Clause]) = alignAcrossRows(f map (_.toList) toList, "\\/", " /\\\n")

    import scala.collection.mutable.ArrayBuffer

    // a clause is a disjunction of distinct literals
    def clause(l: Lit*): Clause = l.toSet

    // adapted from http://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)
    val EmptyModel = Map.empty[Sym, Boolean]
    val NoModel: Model = null

    // this model contains the auxiliary variables as well
    type TseitinModel = Set[Lit]
    val EmptyTseitinModel = Set.empty[Lit]
    val NoTseitinModel: TseitinModel = null

    // http://docs.oracle.com/javase/7/docs/api/java/lang/System.html#nanoTime() recommends nanoTime for measuring elapsed time
    @inline private def reachedTime(stoppingNanos: Long): Boolean = stoppingNanos != 0 && (stoppingNanos - System.nanoTime < 0)
    private def stoppingNanosFor(millis: Long): Long = if (millis == 0) 0 else System.nanoTime + (millis * 1000000)

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(solvable: Solvable): List[Model] = {
      val stoppingNanos: Long = stoppingNanosFor(AnalysisBudget.defaultTimeoutMillis)

      // variables of the problem
      val allVars: Set[Lit] = solvable.cnf.allLiterals
      // debug.patmat("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      // (i.e. the blocking clause - used for ALL-SAT)
      def negateModel(m: TseitinModel) = m.map(lit => -lit)

      def findAllModels(clauses: Array[Clause], models: List[TseitinModel], recursionDepthAllowed: Int = 10): List[TseitinModel] =
        if (recursionDepthAllowed == 0) models
        else {
          debug.patmat("find all models for\n" + cnfString(clauses))
          val model = findTseitinModelFor(clauses, stoppingNanos)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoTseitinModel) {
            val unassigned = (allVars -- model.map(lit => Lit(lit.variable))).toList
            debug.patmat(s"unassigned ${unassigned map (lit => solvable.symForVar(lit.variable))} in ${projectToModel(model, solvable.symForVar)}")

            def force(lit: Lit) = {
              val model = withLit(findTseitinModelFor(dropUnit(clauses.toArray, lit), stoppingNanos), lit)
              if (model ne NoTseitinModel) List(model)
              else Nil
            }
            val forced = unassigned flatMap { l =>
              force(Lit(l.variable)) ++ force(Lit(-l.variable))
            }
            debug.patmat("forced " + forced)

            val negated = negateModel(model)
            findAllModels(clauses :+ negated, model :: (forced ++ models), recursionDepthAllowed - 1)
          }
          else models
        }

      val tseitinModels = findAllModels(solvable.cnf.clauses, Nil)
      val models = tseitinModels.map(projectToModel(_, solvable.symForVar))
      debug.patmat(s"cnf: ${solvable.cnf.dimacs}")
      debug.patmat(s"models: ${models.mkString(",")}")
      models
    }

    private def withLit(res: TseitinModel, l: Lit): TseitinModel = {
      if (res eq NoTseitinModel) NoTseitinModel else res + l
    }

    /** Drop trivially true clauses, simplify others by dropping negation of `unitLit`.
     *
     *  Disjunctions that contain the literal we're making true in the returned model are trivially true.
     *  Clauses can be simplified by dropping the negation of the literal we're making true
     *  (since False \/ X == X)
     */
    private def dropUnit(clauses: Array[Clause], unitLit: Lit): Array[Clause] = {
      val negated = -unitLit
      val simplified = new ArrayBuffer[Clause](clauses.size)
      clauses foreach {
        case trivial if trivial contains unitLit => // drop
        case clause                              => simplified += clause - negated
      }
      simplified.toArray
    }

    def findModelFor(solvable: Solvable): Model = {
      val stoppingNanos: Long = stoppingNanosFor(AnalysisBudget.defaultTimeoutMillis)
      projectToModel(findTseitinModelFor(solvable.cnf.clauses, stoppingNanos), solvable.symForVar)
    }

    def findTseitinModelFor(clauses: Array[Clause], stoppingNanos: Long): TseitinModel = {
      @inline def orElse(a: TseitinModel, b: => TseitinModel) = if (a ne NoTseitinModel) a else b

      if (reachedTime(stoppingNanos)) throw AnalysisBudget.timeout

      debug.patmat(s"DPLL\n${cnfString(clauses)}")

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaDPLL) else null

      val satisfiableWithModel: TseitinModel =
        if (clauses isEmpty) EmptyTseitinModel
        else if (clauses exists (_.isEmpty)) NoTseitinModel
        else clauses.find(_.size == 1) match {
          case Some(unitClause) =>
            val unitLit = unitClause.head
            withLit(findTseitinModelFor(dropUnit(clauses, unitLit), stoppingNanos), unitLit)
          case _ =>
            // partition symbols according to whether they appear in positive and/or negative literals
            // SI-7020 Linked- for deterministic counter examples.
            val pos = new mutable.LinkedHashSet[Int]()
            val neg = new mutable.LinkedHashSet[Int]()
            clauses.foreach {
              _.foreach {
                lit =>
                  if (lit.positive) pos += lit.variable else neg += lit.variable
              }
            }

            // appearing in both positive and negative
            val impures: mutable.LinkedHashSet[Int] = pos intersect neg

            // appearing only in either positive/negative positions
            val pures: mutable.LinkedHashSet[Int] = (pos ++ neg) -- impures

            if (pures nonEmpty) {
              val pureVar: Int = pures.head
              // turn it back into a literal
              // (since equality on literals is in terms of equality
              //  of the underlying symbol and its positivity, simply construct a new Lit)
              val pureLit = Lit(if (neg(pureVar)) -pureVar else pureVar)
              // debug.patmat("pure: "+ pureLit +" pures: "+ pures +" impures: "+ impures)
              val simplified = clauses.filterNot(_.contains(pureLit))
              withLit(findTseitinModelFor(simplified, stoppingNanos), pureLit)
            } else {
              val split = clauses.head.head
              // debug.patmat("split: "+ split)
              orElse(findTseitinModelFor(clauses :+ clause(split), stoppingNanos), findTseitinModelFor(clauses :+ clause(-split), stoppingNanos))
            }
        }

      if (Statistics.canEnable) Statistics.stopTimer(patmatAnaDPLL, start)

      satisfiableWithModel
    }

    private def projectToModel(model: TseitinModel, symForVar: Map[Int, Sym]): Model =
      if (model == NoTseitinModel) NoModel
      else model collect {
        case lit if symForVar isDefinedAt lit.variable => (symForVar(lit.variable), lit.positive)
      } toMap
  }
}
