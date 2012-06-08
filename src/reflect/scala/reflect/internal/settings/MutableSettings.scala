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
  type BooleanSetting <: Setting { type T = Boolean }
  type IntSetting <: Setting { type T = Int }

  // basically this is a value which remembers if it's been modified
  trait SettingValue extends AbsSettingValue {
    protected var v: T
    protected var setByUser: Boolean = false

    def postSetHook(): Unit = ()
    def isDefault = !setByUser
    def isSetByUser = setByUser
    def value: T = v
    def value_=(arg: T) = {
      setByUser = true
      v = arg
      postSetHook()
    }
  }

  def overrideObjects: BooleanSetting
  def printtypes: BooleanSetting
  def debug: BooleanSetting
  def Ynotnull: BooleanSetting
  def explaintypes: BooleanSetting
  def verbose: BooleanSetting
  def uniqid: BooleanSetting
  def Yshowsymkinds: BooleanSetting
  def Xprintpos: BooleanSetting
  def Yrecursion: IntSetting
  def maxClassfileName: IntSetting
  def Xexperimental: BooleanSetting
  def XoldPatmat: BooleanSetting
  def XnoPatmatAnalysis: BooleanSetting
}