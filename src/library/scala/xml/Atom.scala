/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

/** The class <code>Atom</code> provides an XML node for text (PCDATA).
 *  It is used in both non-bound and bound XML representations.
 *
 *  @author Burak Emir
 *  @param text the text contained in this node, may not be <code>null</code>.
 */
@serializable
class Atom[+A](val data: A) extends SpecialNode {

  data.asInstanceOf[AnyRef] match {
    case null => new IllegalArgumentException("cannot construct Atom(null)")
    case _ =>
  }
  final override def typeTag$: Int = -1

  /** the constant "#PCDATA"
   */
  def label = "#PCDATA"

  override def equals(x: Any) = x match {
    case s:Atom[_] => data == s.data
    case _         => false
  }

  /** hashcode for this Text */
  override def hashCode() =
    data.hashCode()

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ...
   */
  def buildString(sb: StringBuilder) =
    Utility.escape(data.toString(), sb)

  override def text: String = data.toString()

}
