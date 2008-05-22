class Top {
  val o = "Hi there!"
  var v = "Hi there!"
  type T
  val x: T
}

class Bot extends Top {
  override val o = "Ha! " + super.o
  val y: super.T = x
  super.v = "aa"
  println(super.v)
}
