/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal
package settings

/** A Settings abstraction boiled out of the original highly mutable Settings
 *  class with the intention of creating an ImmutableSettings which can be used
 *  interchangeably.   Except of course without the mutants.
 */

trait AbsSettings {
  type Setting <: AbsSettingValue      // Fix to the concrete Setting type

  trait AbsSettingValue {
    type T <: Any
    def value: T
    def isDefault: Boolean
  }
}

