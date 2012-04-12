object Test extends App {
  Macros.fooEval({ println("in ur logz"); "world"})
  println("======================")
  Macros.fooValue({ println("i can has cheezburger?"); "world"})
}