object Test extends App {
  class Foo { 
    override def equals(that: Any) = {
      println("Foo.equals called")
      super.equals(that)
    }
  }

  println(new Foo == new Foo)

  case class Bar(x: Foo)
  val b = new Bar(new Foo)

  // this should not call Foo.equals, but simply compare object identiy of b
  println(b == b)
}
