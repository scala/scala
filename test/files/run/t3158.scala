object Test {
  def main(args: Array[String]) {
    println(args.map(_ => foo _).deep) 
  }

  def foo(xs: String*) {
    println(xs)
  }
}
