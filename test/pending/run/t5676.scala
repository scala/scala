



class Bar[T]


class Foo[T] {
  object A extends Bar[T]
}


class Baz[S] extends Foo[S] {
  override object A extends Bar[S]
}


object Test {
  
  def main(a: Array[String]) {
    val b = new Baz[Any]
    println(b)
  }
  
}
