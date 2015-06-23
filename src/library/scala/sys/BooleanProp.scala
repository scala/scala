/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package sys

import scala.language.implicitConversions

/** A few additional conveniences for Boolean properties.
 */
trait BooleanProp extends Prop[Boolean] {
  /** The semantics of value are determined at Prop creation.  See methods
   *  `valueIsTrue` and `keyExists` in object BooleanProp for examples.
   *
   *  @return   true if the current String is considered true, false otherwise
   */
  def value: Boolean

  /** Alter this property so that `value` will be true. */
  def enable(): Unit

  /** Alter this property so that `value` will be false. */
  def disable(): Unit

  /** Toggle the property between enabled and disabled states. */
  def toggle(): Unit
}

object BooleanProp {
  private[sys]
  class BooleanPropImpl(key: String, valueFn: String => Boolean) extends PropImpl(key, valueFn) with BooleanProp {
    override def setValue[T1 >: Boolean](newValue: T1): Boolean = newValue match {
      case x: Boolean if !x   => val old = value ; clear() ; old
      case x                  => super.setValue(newValue)
    }
    def enable()  = this setValue true
    def disable() = this.clear()
    def toggle()  = if (value) disable() else enable()
  }
  private[sys]
  class ConstantImpl(val key: String, val value: Boolean) extends BooleanProp {
    val isSet = value
    def set(newValue: String) = "" + value
    def setValue[T1 >: Boolean](newValue: T1): Boolean = value
    def get: String = "" + value
    val clear, enable, disable, toggle = ()
    def option = if (isSet) Some(value) else None
    //def or[T1 >: Boolean](alt: => T1): T1 = if (value) true else alt

    protected def zero = false
  }

  /** The java definition of property truth is that the key be in the map and
   *  the value be equal to the String "true", case insensitively.  This method
   *  creates a BooleanProp instance which adheres to that definition.
   *
   *  @return   A BooleanProp which acts like java's Boolean.getBoolean
   */
  def valueIsTrue[T](key: String): BooleanProp = new BooleanPropImpl(key, _.toLowerCase == "true")

  /** As an alternative, this method creates a BooleanProp which is true
   *  if the key exists in the map and is not assigned a value other than "true",
   *  compared case-insensitively, or the empty string.  This way -Dmy.property
   *  results in a true-valued property, but -Dmy.property=false does not.
   *
   *  @return   A BooleanProp with a liberal truth policy
   */
  def keyExists[T](key: String): BooleanProp = new BooleanPropImpl(key, s => s == "" || s.equalsIgnoreCase("true"))

  /** A constant true or false property which ignores all method calls.
   */
  def constant(key: String, isOn: Boolean): BooleanProp = new ConstantImpl(key, isOn)

  implicit def booleanPropAsBoolean(b: BooleanProp): Boolean = b.value
}
