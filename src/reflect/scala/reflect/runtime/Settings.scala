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

  val Xexperimental     = new BooleanSetting(false)
  val XfullLubs         = new BooleanSetting(false)
  val XnoPatmatAnalysis = new BooleanSetting(false)
  val strictInference   = new BooleanSetting(false)
  val Xprintpos         = new BooleanSetting(false)
  val Yposdebug         = new BooleanSetting(false)
  val Yrangepos         = new BooleanSetting(false)
  val Yshowsymowners    = new BooleanSetting(false)
  val Yshowsymkinds     = new BooleanSetting(false)
  val breakCycles       = new BooleanSetting(false)
  val debug             = new BooleanSetting(false)
  val developer         = new BooleanSetting(false)
  val explaintypes      = new BooleanSetting(false)
  val overrideObjects   = new BooleanSetting(false)
  val printtypes        = new BooleanSetting(false)
  val uniqid            = new BooleanSetting(false)
  val verbose           = new BooleanSetting(false)
  val YpartialUnification = new BooleanSetting(false)

  val Yrecursion        = new IntSetting(0)
  val maxClassfileName  = new IntSetting(255)
  def isScala211        = true
  def isScala212        = true
}
