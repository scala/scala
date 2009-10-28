/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import collection.mutable.StringBuilder

object Text {
  def apply(data: String) =
    if (data != null) new Text(data)
    else throw new IllegalArgumentException("tried to construct Text with null")

  def unapply(other: Any) = other match {
    case x: Text  => Some(x.data)
    case _        => None
  }
}

/** The class <code>Text</code> implements an XML node for text (PCDATA).
 *  It is used in both non-bound and bound XML representations.
 *
 *  @author Burak Emir
 *
 *  @param text the text contained in this node, may not be null.
 */
class Text(data: String) extends Atom[String](data)
{
  if (data == null)
    throw new IllegalArgumentException("tried to construct Text with null")

  /** XXX More hashCode flailing. */
  final override def equals(x: Any) = x match {
    case s:String  => s == data
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
