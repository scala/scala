/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

/** The Settings class for runtime reflection.
 *  This should be refined, so that settings are settable via command
 *  line options or properties.
 */
private[reflect] class Settings extends MutableSettings {

  trait Setting extends SettingValue { }

  class BooleanSetting(x: Boolean) extends Setting {
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

  val XnoPatmatAnalysis = new BooleanSetting(false)
  val Vprintpos         = new BooleanSetting(false)
  val Yposdebug         = new BooleanSetting(false)
  val Yrangepos         = new BooleanSetting(false)
  val Yshowsymowners    = new BooleanSetting(false)
  val Yshowsymkinds     = new BooleanSetting(false)
  val breakCycles       = new BooleanSetting(false)
  val debug             = new BooleanSetting(false)
  val developer         = new BooleanSetting(false)
  val explaintypes      = new BooleanSetting(false)
  val printtypes        = new BooleanSetting(false)
  val uniqid            = new BooleanSetting(false)
  val verbose           = new BooleanSetting(false)

  val YhotStatisticsEnabled = new BooleanSetting(false)
  val YstatisticsEnabled    = new BooleanSetting(false)

  val Yrecursion        = new IntSetting(0)
  def isScala212        = true
  private[scala] def isScala213 = true

  @deprecated("Use Vprintpos instead", "2.13.1")
  val Xprintpos: BooleanSetting = Vprintpos
}
