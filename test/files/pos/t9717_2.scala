class Ann(implicit val i: Int)

abstract class Bob(implicit i: Int) extends Ann {
  def foo: Int
  def dee(): Bob = new Bob {
    def foo = 23
  }
}
