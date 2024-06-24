
trait T {
  val f = if (_) 42 else 17

  val p = while (_) println()  // weird

  val q = do println while (_) // weird

  val g = (b: () => Boolean) => while (b()) println()    // less weird
}

import language.implicitConversions

class Setting {
  def tap(f: this.type => Unit): this.type = { f(this); this }
}
class BooleanSetting extends Setting {
  def tap2(f: BooleanSetting => Unit): BooleanSetting = { f(this); this }
}
object BooleanSetting {
  implicit def cv(s: BooleanSetting): Boolean = true
}

object Test extends App {
  val setting = new Setting().tap(println)
  val boolean = new BooleanSetting().tap(if (_) println("yes"))
  val bool1   = new BooleanSetting().tap(s => if (s) println("yes"))
  val bool2a  = new BooleanSetting().tap2(s => if (s) println("yes"))
  val bool2b  = new BooleanSetting().tap2(if (_) println("yes"))
}
