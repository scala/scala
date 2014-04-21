package some.pack

class SomeType(arg: String) {

  type TypeAlias = String

  def >@<(): TypeAlias = "Tricky method name"

  def >#<(): Int = 1

}

object SomeType {

  val someVal = "Some arbitrary companion object value"

}
