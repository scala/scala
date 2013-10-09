object Test {
  def foo[B](f: (=> Int) => B): () => B = () => f(0)

  def main(args: Array[String]) {
    foo(identity)()
  }
}
