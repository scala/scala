/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect.internal
package settings

/** Abstract settings, which is refined for `reflect` and `nsc`.
 */
trait AbsSettings {

  /** Subclasses should further constrain Setting. */
  type Setting <: AbsSettingValue

  /** A setting is a value which may have been specified by the user. */
  trait AbsSettingValue {
    type T <: Any
    def value: T
    def isDefault: Boolean
  }
}
