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

  /** always Node.EmptyNamespace */
  final def namespace = Node.EmptyNamespace;

 /** always empty */
  final def attribute = Node.NoAttributes;

  /** always empty */
  final def child = Nil;

}
