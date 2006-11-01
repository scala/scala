abstract class Foo {
  trait Node {
    def ast: AnyRef = null
  }
  trait Something extends Node {
    override def ast = return null
  }
}
