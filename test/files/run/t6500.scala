object Test extends App {
  class Box(val value: Int) extends AnyVal

  trait Foo {
    def append(box: Box): Foo
  }

  class Bar extends Foo {
    override def append(box: Box): Bar = this // produces bad forwarder
  }

  ((new Bar): Foo).append(new Box(0))
}
