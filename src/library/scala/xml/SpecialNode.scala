/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

/** a special XML node is either text (PCDATA), a comment, a PI, or
 *  an entity ref
**/
abstract class SpecialNode extends Node {

  /** always empty */
  final override def attributes = Null;

  /** always Node.EmptyNamespace */
  final override def namespace = null;

  /** always empty */
  final def child = Nil;

  final override def toString(): String =
    toString(new StringBuffer()).toString();

  def toString(sb:StringBuffer): StringBuffer ;

}
