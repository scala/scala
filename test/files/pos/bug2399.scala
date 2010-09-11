trait That1[A]
trait That2[A, R <: That2[A, R]]

trait T[A, This >: Null <: That1[A] with T[A, This]] extends That2[A, This] {
  self: This =>

  private var next: This = _
  def isEmpty = next eq null

  def length: Int = {
    def loop(x: This, cnt: Int): Int = if (x.isEmpty) cnt else loop(x.next, cnt + 1)
    loop(self, 0)
  }
}