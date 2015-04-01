object Test extends App {
  println(new A_1.Inner())

  // Accessing foo or Deeper triggers the error of SI-9111.
  // However, when not referring to those definitions, compilation should
  // succeed, also if the inliner is enabled.

  // println(i.foo(null))
  // new i.Deeper()
}
