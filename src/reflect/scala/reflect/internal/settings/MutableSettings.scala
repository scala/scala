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

// $Id$

package scala
package reflect.internal
package settings

import scala.reflect.internal.util.StatisticsStatics

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

  def async: BooleanSetting
  def XnoPatmatAnalysis: BooleanSetting
  def Xprintpos: BooleanSetting
  def Yposdebug: BooleanSetting
  def Yrangepos: BooleanSetting
  def Yshowsymowners: BooleanSetting
  def Yshowsymkinds: BooleanSetting
  def breakCycles: BooleanSetting
  def debug: BooleanSetting
  def developer: BooleanSetting
  def explaintypes: BooleanSetting
  def printtypes: BooleanSetting
  def uniqid: BooleanSetting
  def verbose: BooleanSetting

  def YhotStatisticsEnabled: BooleanSetting
  def YstatisticsEnabled: BooleanSetting

  def Yrecursion: IntSetting
}

object MutableSettings {
  import scala.language.implicitConversions
  /** Support the common use case, `if (settings.debug) println("Hello, martin.")`.
   *
   *  Unfortunately, due to the way the `Settings` hierarchy is structured, this abstraction incurs boxing.
   *  Although boxing the Boolean primitive may be a trivial cost for a single invocation,
   *  it is significant for a test in a hot spot. Therefore, this method is deprecated.
   *  For the convenience of plugin authors, it has not been removed outright.
   */
  @deprecated("Use `setting.value` directly to avoid boxing.", since="2.13.9")
  @inline implicit def reflectSettingToBoolean(s: MutableSettings#BooleanSetting): Boolean = s.value

  implicit class SettingsOps(private val settings: MutableSettings) extends AnyVal {
    @inline final def areStatisticsEnabled = (StatisticsStatics.COLD_STATS_GETTER.invokeExact(): Boolean) && settings.YstatisticsEnabled.value
    @inline final def areHotStatisticsEnabled = (StatisticsStatics.HOT_STATS_GETTER.invokeExact(): Boolean) && settings.YhotStatisticsEnabled.value
    @inline final def isDebug: Boolean     = (StatisticsStatics.DEBUG_GETTER.invokeExact(): Boolean) && settings.debug.value
    @inline final def isDeveloper: Boolean = (StatisticsStatics.DEVELOPER_GETTER.invokeExact(): Boolean) && settings.developer.value
  }
}
