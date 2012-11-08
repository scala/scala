/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** `SpecialNode` is a special XML node which represents either text
 *  `(PCDATA)`, a comment, a `PI`, or an entity ref.
 *
 *  `SpecialNode`s also play the role of [[scala.xml.pull.XMLEvent]]s for
 *  pull-parsing.
 *
 *  @author Burak Emir
 */
abstract class SpecialNode extends Node with pull.XMLEvent {

  /** always empty */
  final override def attributes = Null

  /** always Node.EmptyNamespace */
  final override def namespace = null

  /** always empty */
  final def child = Nil

  /** Append string representation to the given string buffer argument. */
  def buildString(sb: StringBuilder): StringBuilder
}
