/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.util

/** A convenience trait for simplifying hashCode creation.
 *  Mix this into a class and define val hashValues = List(x1, x2, ...)
 *  and your hashCode will be derived from those values.  If you define
 *  equals in terms of equalHashValues then your hashCode and equals
 *  methods will never be out of sync.  Something like:
 *
 *  override def equals(other: Any) = other match {
 *    case x: YourClass => this equalHashValues x
 *    case _            => false
 *  }
 *
 * @author Paul Phillips
 */
abstract trait Hashable extends AnyRef
{
  import Hashable._
  protected def hashValues: List[Any]  // in an ideal universe this would be more like List[Hashable]
  protected def hashSeed: Int = 0

  override def hashCode: Int =
    (hashValues map calculateHashCode).foldLeft(hashSeed)((x, y) => x * 41 + y)

  protected def equalHashValues(other: Any) = other match {
    case x: Hashable  => hashValues == x.hashValues
    case _            => false
  }
}
abstract trait StrictHashable extends Hashable
{
  protected def hashValues: List[Hashable]
}

object Hashable
{
  /** This implicit is for StrictHashable's benefit, so your hashValues list
   *  can contain both explicitly Hashable classes and value types.
   */
  implicit def anyVal2Hashable(x: AnyVal): Hashable =
    new Hashable { protected def hashValues = List(x) }

  private def calculateHashCode(x: Any) = x match {
    case null       => 0
    case x: AnyRef  => x.hashCode
    case x          => x.asInstanceOf[AnyRef].hashCode
  }
}