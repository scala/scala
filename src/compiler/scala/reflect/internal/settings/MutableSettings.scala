/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.reflect.internal
package settings

/** A mutable Settings object.
 */
abstract class MutableSettings extends AbsSettings {

  type Setting <: SettingValue

  // basically this is a value which remembers if it's been modified
  trait SettingValue extends AbsSettingValue {
    protected var v: T = _
    protected var setByUser: Boolean = false
    def postSetHook(): Unit = {}

    def isDefault: Boolean = !setByUser
    def value: T = v
    def value_=(arg: T) = {
      setByUser = true
      v = arg
      postSetHook()
    }
  }

  def debug: SettingValue { type T = Boolean }
  def YdepMethTpes: SettingValue { type T = Boolean }
  def Ynotnull: SettingValue { type T = Boolean }
  def explaintypes: SettingValue { type T = Boolean }
  def verbose: SettingValue { type T = Boolean }
  def uniqid: SettingValue { type T = Boolean }
  def Xprintpos: SettingValue { type T = Boolean }
  def printtypes: SettingValue { type T = Boolean }
  def Yrecursion: SettingValue { type T = Int }
  def maxClassfileName: SettingValue { type T = Int }
}