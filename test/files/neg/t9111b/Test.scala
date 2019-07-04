object Test extends App {
  val i = new A.Inner()
  val j = new i.Deeper()
  println(j.foo(new A.T())) // compiles in mixed compilation (it should not)
}
