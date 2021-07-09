class Open

class Door[State] {
  def close[Phantom >: State <: Open]: Int = 0
  def open[Phantom >: State <: Open](): Int = 0
}

class Test {
  val door = new Door[AnyRef]
  // the error here happens later (at refchecks)
  println(door.close.toString)
  // the errors below happen when typing implicit conversions
  println(door.close.toString())
  println(door.close == 0)
  println(door.open().toString)
  println(door.open().toString())
  println(door.open() == 0)
}
