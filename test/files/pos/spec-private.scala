class Foo {

  def foo[@specialized(Int) T](x: T) = new Object {
    private final val myEdges = List(1, 2 , 3)

    def boo {
      myEdges
    }
  }
}
