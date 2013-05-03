/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util

import java.lang.InheritableThreadLocal

/** `DynamicVariables` provide a binding mechanism where the current
 *  value is found through dynamic scope, but where access to the
 *  variable itself is resolved through static scope.
 *
 *  The current value can be retrieved with the value method. New values
 *  should be pushed using the `withValue` method. Values pushed via
 *  `withValue` only stay valid while the `withValue`'s second argument, a
 *  parameterless closure, executes. When the second argument finishes,
 *  the variable reverts to the previous value.
 *
 *  {{{
 *  someDynamicVariable.withValue(newValue) {
 *    // ... code called in here that calls value ...
 *    // ... will be given back the newValue ...
 *  }
 *  }}}
 *
 *  Each thread gets its own stack of bindings.  When a
 *  new thread is created, the `DynamicVariable` gets a copy
 *  of the stack of bindings from the parent thread, and
 *  from then on the bindings for the new thread
 *  are independent of those for the original thread.
 *
 *  @author  Lex Spoon
 *  @version 1.1, 2007-5-21
 */
class DynamicVariable[T](init: T) {
  private val tl = new InheritableThreadLocal[T] {
    override def initialValue = init.asInstanceOf[T with AnyRef]
  }

  /** Retrieve the current value */
  def value: T = tl.get.asInstanceOf[T]

  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the variable
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    tl set newval

    try thunk
    finally tl set oldval
  }

  /** Change the currently bound value, discarding the old value.
    * Usually withValue() gives better semantics.
    */
  def value_=(newval: T) = tl set newval

  override def toString: String = "DynamicVariable(" + value + ")"
}
