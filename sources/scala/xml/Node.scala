/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;


/** Trait for representation of XML elements. These are created by
 *  a dtd2scala binding tool
 */
trait Node {
  /** the label of this XML node */
    def label: String;
  /** the children of this XML node */
    def children: Seq[Node];
  /** the string representation of this XML node */
    def toXML: String;
}
