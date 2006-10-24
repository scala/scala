/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import compat.StringBuilder
import compat.Platform.IllegalArgumentException

/** an XML node for comments.
 *
 * @author Burak Emir
 * @param text text contained in this node, may not contain "--"
 */

case class Comment(commentText: String) extends SpecialNode {

  final override def typeTag$:Int = -3

  if (commentText.indexOf("--") != -1)
    throw new IllegalArgumentException("text containts \"--\"")

  /** structural equality */
  override def equals(x: Any): Boolean = x match {
    case Comment(x) => x.equals(commentText)
    case _ => false
  }

  /** the constant &quot;#REM&quot; */
  def label = "#REM"

  /** hashcode for this Comment */
  override def hashCode() = commentText.hashCode()

  override def text = ""

  /** appends &quot;<!-- text -->&quot; to this stringbuffer */
  def toString(sb: StringBuilder) =
    sb.append("<!--").append(commentText).append("-->")
}
