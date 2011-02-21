object Test extends App {
  trait Happy { val status = "happy" }
  trait Sad { val status = "sad" }

  def go1 = (new AnyRef with Happy with Sad { override val status = "happysad" }).status
  def go2 = (new AnyRef with Happy with Sad { val blurp = "happysad" ; override val status = blurp }).status
  def go3 = (new AnyRef with Happy with Sad { override val status = blurp ; val blurp = "happysad" }).status
}

