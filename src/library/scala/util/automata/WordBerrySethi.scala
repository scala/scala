/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.automata

import scala.collection.{ immutable, mutable, Map }
import immutable.{ Set }
import mutable.{ HashSet, HashMap }
import scala.util.regexp.WordExp

/** This class turns a regexp into a NondetWordAutom using the
 *  celebrated position automata construction (also called Berry-Sethi or
 *  Glushkov)
 *
 *  @author Burak Emir
 *  @version 1.0
 */
abstract class WordBerrySethi extends BaseBerrySethi {
  override val lang: WordExp

  import lang.{ Alt, Eps, Letter, Meta, RegExp, Sequ, Star }

  protected var labels: HashSet[lang._labelT] = _
  // don't let this fool you, only labelAt is a real, surjective mapping
  protected var labelAt: immutable.Map[Int, lang._labelT] = _ // new alphabet "gamma"

  protected var deltaq: Array[HashMap[lang._labelT, List[Int]]] = _ // delta

  protected var defaultq: Array[List[Int]] = _ // default transitions

  protected var initials: Set[Int] = _

  /** Computes <code>first(r)</code> where the word regexp <code>r</code>.
   *
   *  @param r the regular expression
   *  @return  the computed set <code>first(r)</code>
   */
  protected override def compFirst(r: RegExp): Set[Int] = r match {
    case x: Letter  => Set(x.pos)
    case _          => super.compFirst(r)
  }

  /** Computes <code>last(r)</code> where the word regexp <code>r</code>.
   *
   *  @param r the regular expression
   *  @return  the computed set <code>last(r)</code>
   */
  protected override def compLast(r: RegExp): Set[Int] = r match {
    case x: Letter  => Set(x.pos)
    case _          => super.compLast(r)
  }

  /** Returns the first set of an expression, setting the follow set along
   *  the way.
   *
   *  @param fol1 ...
   *  @param r    the regular expression
   *  @return     the computed set
   */
  protected override def compFollow1(fol1: Set[Int], r: RegExp): Set[Int] = r match {
      case x: Letter  => follow(x.pos) = fol1 ; Set(x.pos)
      case Eps        => emptySet
      case _          => super.compFollow1(fol1, r)
    }

  /** returns "Sethi-length" of a pattern, creating the set of position
   *  along the way
   */

  /** called at the leaves of the regexp */
  protected def seenLabel(r: RegExp, i: Int, label: lang._labelT) {
    labelAt = labelAt.updated(i, label)
    this.labels += label
  }

  // overriden in BindingBerrySethi
  protected def seenLabel(r: RegExp, label: lang._labelT): Int = {
    pos += 1
    seenLabel(r, pos, label)
    pos
  }

  // todo: replace global variable pos with acc
  override def traverse(r: RegExp): Unit = r match {
    case a @ Letter(label) => a.pos = seenLabel(r, label)
    case Eps               => // ignore
    case _                 => super.traverse(r)
  }


  protected def makeTransition(src: Int, dest: Int, label: lang._labelT) {
    val q = deltaq(src)
    q.update(label, dest :: q.getOrElse(label, Nil))
  }

  protected def initialize(subexpr: Seq[RegExp]): Unit = {
    this.labelAt = immutable.Map()
    this.follow = HashMap()
    this.labels = HashSet()
    this.pos = 0

    // determine "Sethi-length" of the regexp
    subexpr foreach traverse

    this.initials = Set(0)
  }

  protected def initializeAutom() {
    finals   = immutable.Map.empty[Int, Int]                    // final states
    deltaq   = new Array[HashMap[lang._labelT, List[Int]]](pos) // delta
    defaultq = new Array[List[Int]](pos)                        // default transitions

    for (j <- 0 until pos) {
      deltaq(j) = HashMap[lang._labelT, List[Int]]()
      defaultq(j) = Nil
    }
  }

  protected def collectTransitions(): Unit =                // make transitions
    for (j <- 0 until pos ; val fol = follow(j) ; k <- fol) {
      if (pos == k) finals = finals.updated(j, finalTag)
      else makeTransition(j, k, labelAt(k))
    }

  def automatonFrom(pat: RegExp, finalTag: Int): NondetWordAutom[lang._labelT] = {
    this.finalTag = finalTag

    pat match {
      case x: Sequ =>
        // (1,2) compute follow + first
        initialize(x.rs)
        pos += 1
        compFollow(x.rs)  // this used to be assigned to var globalFirst and then never used.

        // (3) make automaton from follow sets
        initializeAutom()
        collectTransitions()

        if (x.isNullable) // initial state is final
          finals = finals.updated(0, finalTag)

        val delta1      = immutable.Map(deltaq.zipWithIndex map (_.swap): _*)
        val finalsArr   = 0 until pos map (k => finals.getOrElse(k, 0)) toArray  // 0 == not final
        val initialsArr = initials.toArray

        val deltaArr: Array[Map[lang._labelT, immutable.BitSet]] =
          (0 until pos map { x =>
            HashMap(delta1(x).toSeq map { case (k, v) => k -> immutable.BitSet(v: _*) } : _*)
          }) toArray

        val defaultArr  = 0 until pos map (k => immutable.BitSet(defaultq(k): _*)) toArray

        new NondetWordAutom[lang._labelT] {
          val nstates  = pos
          val labels   = WordBerrySethi.this.labels.toList
          val initials = initialsArr
          val finals   = finalsArr
          val delta    = deltaArr
          val default  = defaultArr
        }
      case z =>
       automatonFrom(Sequ(z.asInstanceOf[this.lang._regexpT]), finalTag)
    }
  }
}