trait Base {
  val x: Int;
  val y: Int;
  var z: Int;
}

class Sub() extends Base {
  override val x: Int = 1;
  override val y: Int = 2;
  override var z: Int = 3;
}
