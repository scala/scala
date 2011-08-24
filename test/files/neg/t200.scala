trait X {
  def foo: Int;
}

trait Y extends X {
  def foo: String;
  def foo: Int;
}
