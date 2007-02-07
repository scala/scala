/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A sorted map that is backed by a Java tree map.
 *
 * @author Sean McDirmid
 */
class TreeMap[K <% Ordered[K],E] extends SortedMapWrapper[K,E] {
  val underlying = (new java.util.TreeMap(new Comparator[K]));
}
