abstract class Bug360A { self: Bug360C =>
  def f: String = "hello";
}
trait Bug360B { self: Bug360C =>
  object d {
    Console.println(f);
  }
}
abstract class Bug360C extends Bug360A with Bug360B;
