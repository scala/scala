import scala.collection.immutable.Stack

object Test {

  def test = {
    val s = new Stack[Int]
    s.push(1)
    s.push(1, 2)
    s.push(1, 2, 3)
    s.push(1, 2, 3, 4)
  }

}
