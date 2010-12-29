/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.automata

import scala.util.regexp.{ Base }
import scala.collection.{ mutable, immutable }

// todo: replace global variable pos with acc

/** this turns a regexp over A into a NondetWorkAutom over A using the
 *  celebrated position automata construction (also called Berry-Sethi or
 *  Glushkov)
 */
abstract class BaseBerrySethi {
  val lang: Base
  import lang.{ Alt, Eps, Meta, RegExp, Sequ, Star }

  protected var pos = 0

  // results which hold all info for the NondetWordAutomaton
  protected var follow: mutable.HashMap[Int, Set[Int]] = _

  protected var finalTag: Int = _

  protected var finals: immutable.Map[Int, Int] = _     // final states

  // constants --------------------------

  final val emptySet: Set[Int] = Set()

  private def doComp(r: RegExp, compFunction: RegExp => Set[Int]) = r match {
    case x: Alt   => (x.rs map compFirst).foldLeft(emptySet)(_ ++ _)
    case Eps      => emptySet
    case x: Meta  => compFunction(x.r)
    case x: Sequ  =>
      val (l1, l2) = x.rs span (_.isNullable)
      ((l1 ++ (l2 take 1)) map compFunction).foldLeft(emptySet)(_ ++ _)
    case Star(t)  => compFunction(t)
    case _        => throw new IllegalArgumentException("unexpected pattern " + r.getClass())
  }

  /** computes first( r ) for the word regexp r */
  protected def compFirst(r: RegExp): Set[Int] = doComp(r, compFirst)

  /** computes last( r ) for the regexp r */
  protected def compLast(r: RegExp): Set[Int] = doComp(r, compLast)

  /** Starts from the right-to-left
   *  precondition: pos is final
   *               pats are successor patterns of a Sequence node
   *
   *  @param r ...
   *  @return  ...
   */
  protected def compFollow(rs: Seq[RegExp]): Set[Int] = {
    follow(0) =
      if (rs.isEmpty) emptySet
      else rs.foldRight(Set(pos))((p, fol) => {
        val first = compFollow1(fol, p)

        if (p.isNullable) fol ++ first
        else first
      })

    follow(0)
  }

  /** returns the first set of an expression, setting the follow set along
   *  the way.
   *
   *  @param fol1 ...
   *  @param r    ...
   *  @return     ...
   */
  protected def compFollow1(fol1: Set[Int], r: RegExp): Set[Int] = r match {
    case x: Alt     => Set(x.rs reverseMap (compFollow1(fol1, _)) flatten: _*)
    case x: Meta    => compFollow1(fol1, x.r)
    case x: Star    => compFollow1(fol1 ++ compFirst(x.r), x.r)
    case x: Sequ    =>
      var first = emptySet
      x.rs.foldRight(fol1) { (p, fol) =>
        val first = compFollow1(fol, p)

        if (p.isNullable) fol ++ first
        else first
      }
      first
    case _          => throw new IllegalArgumentException("unexpected pattern: " + r.getClass())
  }

  /** returns "Sethi-length" of a pattern, creating the set of position
   *  along the way.
   *
   *  @param r ...
   */
  protected def traverse(r: RegExp): Unit = r match {
    // (is tree automaton stuff, more than Berry-Sethi)
    case x: Alt   => x.rs foreach traverse
    case x: Sequ  => x.rs foreach traverse
    case x: Meta  => traverse(x.r)
    case Star(t)  => traverse(t)
    case _        => throw new IllegalArgumentException("unexp pattern " + r.getClass())
  }
}
