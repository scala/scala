object Test extends App {
  // work around optimizer bug SI-5672  -- generates wrong bytecode for switches in arguments
  // virtpatmat happily emits a switch for a one-case switch
  // this is not the focus of this test, hence the temporary workaround
  def a = (1, 2, 3) match { case (r, \u03b8, \u03c6) => r + \u03b8 + \u03c6 }
  println(a)
  def b = (1 match { case \u00e9 => \u00e9 })
  println(b)
}
