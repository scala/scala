/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

/** <p>
 *    A common class for lightweight sets.
 *  </p>
 *  <dl class="subclasses">
 *    <dt><b>Direct Known Subclasses:</b></dt>
 *    <dd>
 *      <a href="HashSet.html" target="contentFrame">HashSet</a>
 *   </dd>
 *  </dl>
 */
abstract class Set[T <: AnyRef] {

  def findEntry(x: T): T

  def addEntry(x: T): unit

  def elements: Iterator[T]

  def contains(x: T): boolean =
    findEntry(x) != null

  def toList = elements.toList

}
