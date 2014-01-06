class C {
  val x = 1
  object $ {
    val y = x + x
    class abc$ {
      def xy = x + y
    }
    object abc$ {
      def xy = x + y
    }
  }
}

object Test extends App {
  val c = new C()
  println(c.$.y)
  println(c.$.abc$.xy)
  println(new c.$.abc$().xy)
}
