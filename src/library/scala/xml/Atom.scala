/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** The class `Atom` provides an XML node for text (`PCDATA`).
 *  It is used in both non-bound and bound XML representations.
 *
 *  @author Burak Emir
 *  @param data the text contained in this node, may not be `'''null'''`.
 */
class Atom[+A](val data: A) extends SpecialNode with Serializable {
  if (data == null)
    throw new IllegalArgumentException("cannot construct "+getClass.getSimpleName+" with null")

  override protected def basisForHashCode: Seq[Any] = Seq(data)

  override def strict_==(other: Equality) = other match {
    case x: Atom[_] => data == x.data
    case _          => false
  }

  override def canEqual(other: Any) = other match {
    case _: Atom[_] => true
    case _          => false
  }

  final override def doCollectNamespaces = false
  final override def doTransform         = false

  def label = "#PCDATA"

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   */
  def buildString(sb: StringBuilder): StringBuilder =
    Utility.escape(data.toString, sb)

  override def text: String = data.toString

}
