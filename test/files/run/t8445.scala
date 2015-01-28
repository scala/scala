object X {
  class Y
  def y = new Y {
    class Z
    def z = classOf[Z]
  }
}

object Test extends App {
  assert(X.y.z.getEnclosingClass.getName == "X$$anon$1")
}
