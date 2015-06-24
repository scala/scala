class T {
  def foo(o: Tuple1[String]) = ()
}

class U extends T {
   private def foo(o: Tuple1[Any]) = ()
}

object Test {
  def main(args: Array[String]): Unit = {
    new U().foo(null) // IllegalAccessError:  tried to access method U.foo(Lscala/Tuple1;)V from class Test$
  }
}
