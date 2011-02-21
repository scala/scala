object Test extends App {

  def f[A](x : => A) = x

  Console.println(f(Array(42))(0))
}
