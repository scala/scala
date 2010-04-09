/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable


/** This class is typically used as a mixin. It turns maps which map <code>A</code>
 *  to <code>Set[B]</code> objects into multi maps which map <code>A</code> to
 *  <code>B</code> objects.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
trait MultiMap[A, B] extends Map[A, Set[B]] {
  protected def makeSet: Set[B] = new HashSet[B]

  @deprecated("use addBinding instead")
  def add(key: A, value: B): this.type = addBinding(key, value)

  /** Assigns the specified `value` to a specified `key`, replacing
   *  the existing value assigned to that `key` if it is equal to
   *  the specified value. Otherwise, simply adds another binding to
   *  the `key`.
   *
   *  @param key    The key to which to bind the new value.
   *  @param value  The value to bind to the key.
   *  @return       A reference to this multimap.
   */
  def addBinding(key: A, value: B): this.type = {
    get(key) match {
      case None =>
        val set = makeSet
        set += value
        this(key) = set
      case Some(set) =>
        set += value
    }
    this
  }

  /** Removes the binding of `value` to `key` if it exists.
   *
   *  @param key     The key of the binding.
   *  @param value   The value to remove.
   *  @return        A reference to this multimap.
   */
  def removeBinding(key: A, value: B): this.type = {
    get(key) match {
      case None =>
        case Some(set) =>
          set -= value
          if (set.isEmpty) this -= key
    }
    this
  }

  /** Checks if there exists a binding to `key` such that it satisfies the predicate `p`.
   *
   *  @param key   The key for which the predicate is checked.
   *  @param p     The predicate which a value assigned to the key must satisfy.
   *  @return      A boolean if such a binding exists
   */
  def entryExists(key: A, p: B => Boolean): Boolean = get(key) match {
    case None => false
    case Some(set) => set exists p
  }
}
