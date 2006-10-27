abstract class Foo {
  trait Node {
    def ast: Object = null
  }
  trait Something extends Node {
    override def ast = return null
  }
}
