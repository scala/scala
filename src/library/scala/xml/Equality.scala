/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** In an attempt to contain the damage being inflicted on consistency by the
 *  ad hoc `equals` methods spread around `xml`, the logic is centralized and
 *  all the `xml` classes go through the `xml.Equality trait`.  There are two
 *  forms of `xml` comparison.
 *
 *  1. `'''def''' strict_==(other: xml.Equality)`
 *
 *  This one tries to honor the little things like symmetry and hashCode
 *  contracts.  The `equals` method routes all comparisons through this.
 *
 *  1. `xml_==(other: Any)`
 *
 *  This one picks up where `strict_==` leaves off.  It might declare any two
 *  things equal.
 *
 *  As things stood, the logic not only made a mockery of the collections
 *  equals contract, but also laid waste to that of case classes.
 *
 *  Among the obstacles to sanity are/were:
 *
 *    Node extends NodeSeq extends Seq[Node]
 *    MetaData extends Iterable[MetaData]
 *    The hacky "Group" xml node which throws exceptions
 *      with wild abandon, so don't get too close
 *    Rampant asymmetry and impossible hashCodes
 *    Most classes claiming to be equal to "String" if
 *      some specific stringification of it was the same.
 *      String was never going to return the favor.
 */

object Equality {
  def asRef(x: Any): AnyRef = x.asInstanceOf[AnyRef]

  /** Note - these functions assume strict equality has already failed.
   */
  def compareBlithely(x1: AnyRef, x2: String): Boolean = x1 match {
    case x: Atom[_]   => x.data == x2
    case x: NodeSeq   => x.text == x2
    case _            => false
  }
  def compareBlithely(x1: AnyRef, x2: Node): Boolean = x1 match {
    case x: NodeSeq if x.length == 1  => x2 == x(0)
    case _                            => false
  }
  def compareBlithely(x1: AnyRef, x2: AnyRef): Boolean = {
    if (x1 == null || x2 == null)
      return (x1 eq x2)

    x2 match {
      case s: String  => compareBlithely(x1, s)
      case n: Node    => compareBlithely(x1, n)
      case _          => false
    }
  }
}
import Equality._

trait Equality extends scala.Equals {
  def basisForHashCode: Seq[Any]
  def strict_==(other: Equality): Boolean
  def strict_!=(other: Equality) = !strict_==(other)

  /** We insist we're only equal to other `xml.Equality` implementors,
   *  which heads off a lot of inconsistency up front.
   */
  override def canEqual(other: Any): Boolean = other match {
    case x: Equality    => true
    case _              => false
  }

  /** It's be nice to make these final, but there are probably
   *  people out there subclassing the XML types, especially when
   *  it comes to equals.  However WE at least can pretend they
   *  are final since clearly individual classes cannot be trusted
   *  to maintain a semblance of order.
   */
  override def hashCode()         = basisForHashCode.##
  override def equals(other: Any) = doComparison(other, false)
  final def xml_==(other: Any)    = doComparison(other, true)
  final def xml_!=(other: Any)    = !xml_==(other)

  /** The "blithe" parameter expresses the caller's unconcerned attitude
   *  regarding the usual constraints on equals.  The method is thereby
   *  given carte blanche to declare any two things equal.
   */
  private def doComparison(other: Any, blithe: Boolean) = {
    val strictlyEqual = other match {
      case x: AnyRef if this eq x => true
      case x: Equality            => (x canEqual this) && (this strict_== x)
      case _                      => false
    }

    strictlyEqual || (blithe && compareBlithely(this, asRef(other)))
  }
}
