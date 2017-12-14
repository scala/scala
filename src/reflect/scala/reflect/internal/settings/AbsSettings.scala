/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect.internal
package settings

/** A Settings abstraction boiled out of the original highly mutable Settings
 *  class with the intention of creating an ImmutableSettings which can be used
 *  interchangeably.   Except of course without the mutants.
 */

trait AbsSettingValue {
  type T <: Any
  def value: T
  def isDefault: Boolean
}

object AbsSettingValue {
  import scala.language.implicitConversions
  /** Support the common use case, `if (settings.debug) println("Hello, martin.")` */
  @inline implicit def s2v[U](s: AbsSettingValue { type T = U }): U = s.value
}

trait AbsSettings {
  type AbsSettingValue = scala.reflect.internal.settings.AbsSettingValue
  type Setting <: AbsSettingValue      // Fix to the concrete Setting type
}

