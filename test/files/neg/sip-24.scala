
trait X {

  val x  = 42
  val y_ = 17

  val `a..z` = ('a' to 'z').toList

  val x$y = 3

  def `fails but x_ is not alnum`    = s"$x_"   // not emitted b/c not a parse error

  def `compiles but y_ is not alnum` = s"$y_"

  def `brace yourselves`             = s"ABCs: $`a..z`"

  def `embedded dollars`             = s"<$`x$y`"

  def `one style of embedded quote`  = s"$"Don't quote me on that!$" he objected."

  //def `SI-6476 Popular style quote`= s"\"Don't quote me on that!\" he objected."
}
