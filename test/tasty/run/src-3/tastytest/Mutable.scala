package tastytest

class Mutable {
  private var _foo = 33
  def foo_=(foo: Int): Unit = { _foo = foo }
  def foo: Int = _foo

  var bar = 45
}
