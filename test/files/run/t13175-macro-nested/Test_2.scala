package p {
  final class S extends Shape
  final class SP extends Poly

  object T { def run(): Unit = Macros.tree[Shape].foreach(println) }
}

object Test {
  def main(args: Array[String]): Unit = p.T.run()
}
