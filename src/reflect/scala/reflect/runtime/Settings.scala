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
package reflect
package runtime

import scala.reflect.internal.settings.MutableSettings
import scala.reflect.internal.util.StatisticsStatics

/** The Settings class for runtime reflection.
 *  This should be refined, so that settings are settable via command
 *  line options or properties.
 */
private[reflect] class Settings extends MutableSettings {

  trait Setting extends SettingValue { }

  class BooleanSetting(@deprecatedName x: Boolean) extends Setting {
    type T = Boolean
    protected var v: Boolean = x
    override def value: Boolean = v
  }

  class IntSetting(x: Int) extends Setting {
    type T = Int
    protected var v: Int = x
    override def value: Int = v
  }

  class MultiStringSetting(xs: List[String]) extends Setting {
    type T = List[String]
    protected var v: List[String] = xs
    override def value: List[String] = v
  }

  val async             = new BooleanSetting(false)
  val XnoPatmatAnalysis = new BooleanSetting(false)
  val Xprintpos         = new BooleanSetting(false)
  val Yposdebug         = new BooleanSetting(false)
  val Yrangepos         = new BooleanSetting(true)
  val Yshowsymowners    = new BooleanSetting(false)
  val Yshowsymkinds     = new BooleanSetting(false)
  val breakCycles       = new BooleanSetting(false)
  val debug             = new BooleanSetting(false) { override def postSetHook() = if (v) StatisticsStatics.enableDebugAndDeoptimize()     }
  val developer         = new BooleanSetting(false) { override def postSetHook() = if (v) StatisticsStatics.enableDeveloperAndDeoptimize() }
  val explaintypes      = new BooleanSetting(false)
  val printtypes        = new BooleanSetting(false)
  val uniqid            = new BooleanSetting(false)
  val verbose           = new BooleanSetting(false)

  val YhotStatisticsEnabled = new BooleanSetting(false) { override def postSetHook() = if (v && YstatisticsEnabled.value) StatisticsStatics.enableHotStatsAndDeoptimize()  }
  val YstatisticsEnabled    = new BooleanSetting(false) { override def postSetHook() = if (v)                       StatisticsStatics.enableColdStatsAndDeoptimize() }

  val Yrecursion = new IntSetting(0)
  def isScala212 = true
  def isScala213 = true
}
