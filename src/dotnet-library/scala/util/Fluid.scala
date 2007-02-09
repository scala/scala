/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util


import System.Threading.Thread
import System.LocalDataStoreSlot

/** Fluids provide a binding mechanism where the current
  * value is found through <em>dynamic scope</em>, but where
  * access to the fluid itself is resolved through </em>static
  * binding</em> to a variable referencing the fluid.
  *
  * The current value can be retrieved with the
  * <code>value</code> method.  New values can be
  * pushed using the <code>withValue</code> method.
  * Values pushed via <code>withValue</code> only
  * stay valid while the <code>withValue</code>'s
  * <em>second</em> argument, a parameterless closure,
  * executes.  When the second argument finishes,
  * the fluid reverts to the previous value.
  *
  * Usage of <code>withValue</code> looks like this:
  * <blockquote><pre>
  * someFluid.withValue(newValue) {
  *   // ... code called in here that calls value ...
  *   // ... will be given back the newValue ...
  * }
  * </pre></blockquote>
  *
  * Each thread gets its own stack of bindings.  When a
  * new thread is created, the fluid gets a copy of
  * the stack of bindings from the parent thread, and
  * from then on the bindings for the new thread
  * are independent of those for the original thread.
  *
  *  @author  Lex Spoon
  *  @version 1.0, 21/03/2006
  */
class Fluid[T](init: T) {
  private val slot: LocalDataStoreSlot = Thread.AllocateDataSlot()
  value = init

  /** Retrieve the current value */
  def value: T = Thread.GetData(slot).asInstanceOf[T]

  /** Set the value of the fluid while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the fluid
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: T)(thunk: =>S): S = {
    val oldval = value
    value = newval

    try { thunk } finally {
      value = oldval
    }
  }

  /** Change the currently bound value, discarding the old value.
    * Usually withValue() gives better semantics.
    */
  def value_=(newval: T) = { Thread.SetData(slot, newval.asInstanceOf[AnyRef]) }

  override def toString: String = "Fluid(" + value  +")"
}
