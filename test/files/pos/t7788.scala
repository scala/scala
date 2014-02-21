class Test {
  // Predef used to define a method `conforms` to produce the implicit evidence below
  // all this does is ensure we don't rename Predef.$conforms back to conforms when $ goes out of fashion
  // or that there is some other way of generating the implicit value that witnesses T => U for T <: U
  def conforms(x: Int, y: Int) = x < y
  def foo[A](implicit ev: Int => A) = ???
  foo[Int]
}