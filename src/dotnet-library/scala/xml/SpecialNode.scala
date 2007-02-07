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

/** &lt;code&gt;SpecialNode&lt;/code&gt; is a special XML node which
 *  represents either text (PCDATA), a comment, a PI, or an entity ref.
 *  SpecialNodes also play the role of XMLEvents for pull-parsing.
 *  @author Burak Emir
 */
abstract class SpecialNode extends Node with pull.XMLEvent {

  /** always empty */
  final override def attributes = Null

  /** always Node.EmptyNamespace */
  final override def namespace = null

  /** always empty */
  final def child = Nil

  /** append string representation to the given stringbuffer */
  def toString(sb: StringBuilder): StringBuilder

}
