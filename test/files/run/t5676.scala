import java.lang.reflect.Modifier

class Bar[T]

class Foo[T] {
  object A extends Bar[T]
}

class Baz[S] extends Foo[S] {
  override object A extends Bar[S] {
    def foo(): String = "ok"
  }
}

object Test {

  def main(a: Array[String]) {
    val b = new Baz[Any]
    println(b.A.foo())
    println(Modifier.isFinal(classOf[Baz[Any]].getModifiers()))
    println(Modifier.isFinal(Test.getClass.getModifiers()))
  }

}
