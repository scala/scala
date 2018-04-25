object test {
  def method(): Unit = {
    class Foo extends AnyRef {
      object Color {
        object Blue
      }

      class Board {
        val grid = Color.Blue
      }
    }
    new Foo
  }
 }
