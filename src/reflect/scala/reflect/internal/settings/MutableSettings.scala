/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala
package reflect.internal
package settings

/** A mutable Settings object.
 */
abstract class MutableSettings extends AbsSettings {

  type Setting <: SettingValue
  type BooleanSetting <: Setting { type T = Boolean }
  type IntSetting <: Setting { type T = Int }
  type MultiStringSetting <: Setting { type T = List[String] }

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

    /** Returns Some(value) in the case of a value set by user and None otherwise. */
    def valueSetByUser: Option[T] = if (isSetByUser) Some(value) else None
  }

  def Xexperimental: BooleanSetting
  def XfullLubs: BooleanSetting
  def XnoPatmatAnalysis: BooleanSetting
  def Xprintpos: BooleanSetting
  def strictInference: BooleanSetting
  def Yposdebug: BooleanSetting
  def Yrangepos: BooleanSetting
  def Yshowsymowners: BooleanSetting
  def Yshowsymkinds: BooleanSetting
  def breakCycles: BooleanSetting
  def debug: BooleanSetting
  def developer: BooleanSetting
  def explaintypes: BooleanSetting
  def overrideObjects: BooleanSetting
  def printtypes: BooleanSetting
  def uniqid: BooleanSetting
  def verbose: BooleanSetting
  def YpartialUnification: BooleanSetting

  def Yrecursion: IntSetting
  def maxClassfileName: IntSetting

  def isScala211: Boolean
}

object MutableSettings {
  import scala.language.implicitConversions
  /** Support the common use case, `if (settings.debug) println("Hello, martin.")` */
  @inline implicit def reflectSettingToBoolean(s: MutableSettings#BooleanSetting): Boolean = s.value
}
