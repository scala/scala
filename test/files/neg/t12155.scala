class C1[A, B] {
  private class Node[X] {
    def size = 42
  }
}
class D[A, C](val x: C1[A, _]#Node[C]) {
}
class Client {
  def test(d: D[_, _]) = d.x.size
}
