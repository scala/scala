/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.reflect.internal.util.Statistics
import scala.language.postfixOps
import scala.collection.{immutable, mutable}

/**
 * Solve pattern matcher exhaustivity problem via DPLL.
 */
trait Solving extends Logic {

  private def traverse[A, B](a: Set[A])(f: A => Option[B]): Option[Set[B]] = {
    val s = immutable.Set.newBuilder[B]
    val i = a.iterator
    while (i.hasNext) {
      val e = i.next()
      f(e) match {
        case Some(x) =>
          s += x
        case None    =>
          return None
      }
    }
    Some(s.result)
  }

  import PatternMatchingStats._

  /**
   * Tseitin transformation: used for conversion of a
   * propositional formula into conjunctive normal form (CNF)
   * (input format for SAT solver).
   * A simple conversion into CNF via Shannon expansion would
   * also be possible but it's worst-case complexity is exponential
   * (in the number of variables) and thus even simple problems
   * could become untractable.
   * The Tseitin transformation results in an _equisatisfiable_
   * CNF-formula (it generates auxiliary variables)
   * but runs with linear complexity.
   */
  trait TseitinCNF extends PropositionalLogic {

    def eqFreePropToSolvable(p: Prop): Solvable = {
      type Cache = Map[Prop, Lit]

      val cache = mutable.Map[Prop, Lit]()

      val cnf = new CNF

      def convertWithoutCache(p: Prop): Lit = {
        p match {
          case And(fv)   =>
            and(fv.map(convertWithCache))
          case Or(fv)    =>
            or(fv.map(convertWithCache))
          case Not(a)    =>
            not(convertWithCache(a))
          case _: Sym =>
            cnf.newLiteral()
          case True      =>
            cnf.constTrue
          case False     =>
            cnf.constFalse
          case _: Eq =>
            sys.error("Forgot to call propToSolvable()?")
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
          new_bv.map(op => addClauseProcessed(op.pos, o.neg))
          addClauseProcessed((new_bv.map(_.neg) + o.pos).toSeq: _*)
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
          new_bv.map(op => addClauseProcessed(op.neg, o.pos))
          addClauseProcessed((new_bv.map(_.pos) + o.neg).toSeq: _*)
          o
        }
      }

      // no need for auxiliary variable
      def not(a: Lit): Lit = -a

      object ToLiteral {
        def unapply(f: Prop): Option[Lit] = f match {
          case Not(a)    =>
            ToLiteral.unapply(a).map(_.neg)
          case _: Sym =>
            Some(convertWithCache(f)) // go via cache in order to get single literal for variable
          case True      =>
            Some(cnf.constTrue)
          case False     =>
            Some(cnf.constFalse)
          case _         =>
            None
        }
      }

      object ToDisjunction {
        def unapply(p: Prop): Option[CNF#Clause] = p match {
          case Or(fv) =>
            traverse(fv)(ToLiteral.unapply)
          case p =>
            ToLiteral.unapply(p).map(Set(_))
        }
      }

      /**
       * Checks if propositional formula is already in CNF
       */
      object ToCnf {
        def unapply(f: Prop): Option[Seq[CNF#Clause]] = f match {
          case And(fv) =>
            traverse(fv)(ToDisjunction.unapply).map(_.toSeq)
          case p =>
            ToDisjunction.unapply(p).map(Seq(_))
        }
      }

      val simplified = simplify(p)
      simplified match {
        case ToCnf(clauses) =>
          // already in CNF, just add clauses
          clauses.foreach(cnf.addClauseRaw)
        case p                       =>
          // add intermediate variable since we want the formula to be SAT!
          cnf.addClauseProcessed(convertWithCache(p))
      }

      // all variables are guaranteed to be in cache
      // (doesn't mean they will appear in the resulting formula)
      val symForVar: Map[Int, Sym] = cache.collect {
        case (sym: Sym, lit) => lit.v -> sym
      }(collection.breakOut) // breakOut in order to obtain immutable Map

      Solvable(cnf, symForVar)
    }

  }

  // simple solver using DPLL
  trait Solver extends TseitinCNF {
    type Clause  = CNF#Clause

    /** Override Array creation for efficiency (to not go through reflection). */
    private implicit val clauseTag: scala.reflect.ClassTag[Clause] = new scala.reflect.ClassTag[Clause] {
      def runtimeClass: java.lang.Class[Clause] = classOf[Clause]
      final override def newArray(len: Int): Array[Clause] = new Array[Clause](len)
    }

    def cnfString(f: Formula) = alignAcrossRows(f map (_.toList) toList, "\\/", " /\\\n")

    import scala.collection.mutable.ArrayBuffer
    type FormulaBuilder = ArrayBuffer[Clause]
    def formulaBuilder  = ArrayBuffer[Clause]()
    def formulaBuilderSized(init: Int)  = new ArrayBuffer[Clause](init)
    def addFormula(buff: FormulaBuilder, f: Formula): Unit = buff ++= f
    def toFormula(buff: FormulaBuilder): Formula = buff

    // CNF: a formula is a conjunction of clauses
    type Formula = FormulaBuilder
    def formula(c: Clause*): Formula = ArrayBuffer(c: _*)

    // a clause is a disjunction of distinct literals
    def clause(l: Lit*): Clause = l.toSet

    // adapted from http://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)
    val EmptyModel = Map.empty[Sym, Boolean]
    val NoModel: Model = null

    // this model contains the auxiliary variables as well
    type TseitinModel = Set[Lit]
    val EmptyTseitinModel = Set.empty[Lit]
    val NoTseitinModel: TseitinModel = null

    private def stoppingTimeFor(timeout: Long) = if (timeout == 0) 0 else System.currentTimeMillis() + timeout

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(solvable: Solvable, timeout: Long): List[Model] = {
      import solvable._

      val stoppingTime: Long = stoppingTimeFor(timeout)

      // variables of the problem
      val allVars: Set[Int] = (1 to cnf.noLiterals).toSet
      // debug.patmat("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      // (i.e. the blocking clause - used for ALL-SAT)
      def negateModel(m: TseitinModel) = m.map(_.neg)

      def findAllModels(f: Formula, models: List[TseitinModel], recursionDepthAllowed: Int = 10): List[TseitinModel] =
        if (recursionDepthAllowed == 0) models
        else {
          debug.patmat("find all models for\n" + cnfString(f))
          val model = findTseitinModelFor(f, stoppingTime)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoTseitinModel) {
            val unassigned = (allVars -- model.map(_.v)).toList
            debug.patmat("unassigned " + unassigned + " in " + model)
            def force(lit: Lit) = {
              val model = withLit(findTseitinModelFor(dropUnit(f.toArray, lit), stoppingTime), lit)
              if (model ne NoTseitinModel) List(model)
              else Nil
            }
            val forced = unassigned flatMap {
              l =>
                force(Lit(l, false)) ++ force(Lit(l, true))
            }
            debug.patmat("forced " + forced)
            val negated = negateModel(model)
            findAllModels(f :+ negated, model :: (forced ++ models), recursionDepthAllowed - 1)
          }
          else models
        }

      val tseitinModels = findAllModels(cnf.buff, Nil)
      val models = tseitinModels.map(projectToModel(_, symForVar))
      debug.patmat(s"cnf: ${cnf.dimacs}")
      debug.patmat(s"models: ${models.mkString(",")}")
      models
    }

    private def withLit(res: TseitinModel, l: Lit): TseitinModel = {
      if (res eq NoTseitinModel) NoTseitinModel else res + l
    }

    private def dropUnit(f: Array[CNF#Clause], unitLit: Lit): Formula = {
      val negated = -unitLit
      // drop entire clauses that are trivially true
      // (i.e., disjunctions that contain the literal we're making true in the returned model),
      // and simplify clauses by dropping the negation of the literal we're making true
      // (since False \/ X == X)
      val dropped = formulaBuilderSized(f.size)
      for {
        clause <- f
        if !(clause contains unitLit)
      } dropped += (clause - negated)
      dropped
    }

    def findModelFor(solvable: Solvable, timeout: Long): Model = {
      val stoppingTime: Long = stoppingTimeFor(timeout)
      projectToModel(findTseitinModelFor(solvable.cnf.buff, stoppingTime), solvable.symForVar)
    }

    def findTseitinModelFor(f: Formula, stoppingTime: Long): TseitinModel = {
      @inline def orElse(a: TseitinModel, b: => TseitinModel) = if (a ne NoTseitinModel) a else b

      if(stoppingTime != 0 && System.currentTimeMillis() > stoppingTime) throw AnalysisBudget.timeout

      debug.patmat(s"DPLL\n${cnfString(f)}")

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaDPLL) else null

      val satisfiableWithModel: TseitinModel =
        if (f isEmpty) EmptyTseitinModel
        else if (f exists (_.isEmpty)) NoTseitinModel
        else f.find(_.size == 1) match {
          case Some(unitClause) =>
            val unitLit = unitClause.head
            withLit(findTseitinModelFor(dropUnit(f.toArray, unitLit), stoppingTime), unitLit)
          case _                =>
            // partition symbols according to whether they appear in positive and/or negative literals
            val pos = new mutable.HashSet[Int]()
            val neg = new mutable.HashSet[Int]()
            f.foreach {
              _.foreach {
                lit =>
                  if (!lit.sign) pos += lit.v else neg += lit.v
              }
            }
            // appearing in both positive and negative
            val impures: mutable.HashSet[Int] = pos intersect neg
            // appearing only in either positive/negative positions
            val pures: mutable.HashSet[Int] = (pos ++ neg) -- impures

            if (pures nonEmpty) {
              val pureVar: Int = pures.head
              // turn it back into a literal
              // (since equality on literals is in terms of equality
              //  of the underlying symbol and its positivity, simply construct a new Lit)
              val pureLit = Lit(pureVar, neg(pureVar))
              // debug.patmat("pure: "+ pureLit +" pures: "+ pures +" impures: "+ impures)
              val simplified = f.filterNot(_.contains(pureLit))
              withLit(findTseitinModelFor(simplified, stoppingTime), pureLit)
            } else {
              val split = f.head.head
              // debug.patmat("split: "+ split)
              orElse(findTseitinModelFor(f :+ clause(split), stoppingTime), findTseitinModelFor(f :+ clause(-split), stoppingTime))
            }
        }

      if (Statistics.canEnable) Statistics.stopTimer(patmatAnaDPLL, start)

      satisfiableWithModel
    }

    private def projectToModel(model: TseitinModel, symForLit: Map[Int, Sym]): Model = {

      if (model eq NoTseitinModel) return NoModel

      (for {
        lit <- model
        sym <- symForLit.get(lit.v)
      } yield {
        sym -> !lit.sign
      }).toMap
    }
  }
}
