/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** The class `Text` implements an XML node for text (PCDATA).
 *  It is used in both non-bound and bound XML representations.
 *
 *  @author Burak Emir
 *  @param text the text contained in this node, may not be null.
 */
class Text(data: String) extends Atom[String](data) {
  if (data == null)
    throw new IllegalArgumentException("tried to construct Text with null")

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   */
  override def buildString(sb: StringBuilder) =
    Utility.escape(data, sb)
}

object Text {
  def apply(data: String) = new Text(data)
  def unapply(other: Any): Option[String] = other match {
    case x: Text  => Some(x.data)
    case _        => None
  }
}
