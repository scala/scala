object Test extends App {
  Macros.foo(y = 1, x = ((x: Int) => x)(2))
  Macros.foo(y = 1, x = {val x = 2; x})
}