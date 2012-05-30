package scala.reflect
package runtime

/** The Settings class for runtime reflection.
 *  This should be refined, so that settings are settable via command
 *  line options or properties.
 */
class Settings extends internal.settings.MutableSettings {

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

  val overrideObjects = new BooleanSetting(false)
  val debug = new BooleanSetting(false)
  val Ynotnull = new BooleanSetting(false)
  val explaintypes = new BooleanSetting(false)
  val verbose = new BooleanSetting(false)
  val uniqid = new BooleanSetting(false)
  val Yshowsymkinds = new BooleanSetting(false)
  val Xprintpos = new BooleanSetting(false)
  val printtypes = new BooleanSetting(false)
  val Yrecursion = new IntSetting(0)
  val maxClassfileName = new IntSetting(255)
  val Xexperimental = new BooleanSetting(false)
  val deepCloning = new BooleanSetting (false)
  val XoldPatmat = new BooleanSetting(false)
  val XnoPatmatAnalysis = new BooleanSetting(false)
}
