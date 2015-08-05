object Test {
  def main(args: Array[String]) {
    println(args.map(_ => foo _).map(_ => "<function1>").deep)
  }

  def foo(xs: String*) {
    println(xs)
  }
}
