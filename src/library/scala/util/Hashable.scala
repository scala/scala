/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util

/** <p>
 *    A convenience trait for simplifying hashCode creation.
 *    Mix this into a class and define <code>val hashValues = Seq(x1, x2, ...)</code>
 *    and your <code>hashCode</code> will be derived from those values.
 *    If you define <code>equals</code> in terms of <code>equalHashValues</code>
 *    then your <code>hashCode</code> and <code>equals</code> methods will
 *    never be out of sync.  Something like:
 *  </p><pre>
 *    <b>override def</b> equals(other: Any) = other <b>match</b> {
 *      <b>case</b> x: YourClass => <b>this</b> equalHashValues x
 *      <b>case</b> _            => <b>false</b>
 *    }</pre>
 *
 * @author Paul Phillips
 */
abstract trait Hashable extends AnyRef
{
  import Hashable._
  protected def hashValues: Seq[Any]  // in an ideal universe this would be more like Seq[Hashable]
  protected def hashSeed: Int = 1

  override def hashCode: Int =
    (hashValues map calculateHashCode).foldLeft(hashSeed)((x, y) => x * 41 + y)

  protected def equalHashValues(other: Any) = other match {
    case x: Hashable  => hashValues sameElements x.hashValues
    case _            => false
  }
}
abstract trait StrictHashable extends Hashable
{
  protected def hashValues: Seq[Hashable]
}

object Hashable
{
  /** This implicit is for StrictHashable's benefit, so your hashValues Seq
   *  can contain both explicitly Hashable classes and value types.
   */
  implicit def anyVal2Hashable(x: AnyVal): Hashable =
    new Hashable { protected def hashValues = Seq(x) }

  private def calculateHashCode(x: Any) = x match {
    case null       => 0
    case x: AnyRef  => x.hashCode
    case x          => x.asInstanceOf[AnyRef].hashCode
  }
}

