/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml


/** The class <code>Text</code> implements an XML node for text (PCDATA).
 *  It is used in both non-bound and bound XML representations.
 *
 *  @author Burak Emir
 *
 *  @param text the text contained in this node, may not be null.
 */
case class Text(_data: String) extends Atom[String](_data) {

  if (null == data)
    throw new java.lang.NullPointerException("tried to construct Text with null")

  final override def equals(x: Any) = x match {
    case s:String  => s == data
    case s:Text    => data == s.data
    case s:Atom[_] => data == s.data
    case _ => false
  }

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ...
   */
  override def buildString(sb: StringBuilder) =
    Utility.escape(data, sb)

}
