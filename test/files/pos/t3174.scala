object test {
  def method() {
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
