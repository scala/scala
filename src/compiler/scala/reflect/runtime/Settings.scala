package scala.reflect
package runtime

class Settings extends internal.settings.MutableSettings {

  class Setting extends SettingValue

  class BooleanSetting(x: Boolean) extends Setting {
    type T = Boolean
    v = x
  }

  class IntSetting(x: Int) extends Setting {
    type T = Int
    v = x
  }

  val debug = new BooleanSetting(false)
  val YdepMethTpes = new BooleanSetting(false)
  val Ynotnull = new BooleanSetting(false)
  val explaintypes = new BooleanSetting(false)
  val verbose = new BooleanSetting(false)
  val uniqid = new BooleanSetting(false)
  val Xprintpos = new BooleanSetting(false)
  val printtypes = new BooleanSetting(false)
  val Yrecursion = new IntSetting(0)
  val maxClassfileName = new IntSetting(255)
}