class A {
  val bippy = 123

  def f = "Put the $bippy in the $bippy!" // warn
}

class B {
  val dingus = 123

  def f = "Put the $bippy in the $bippy!" // no warn
}

class C {
  def f = """Put the ${println("bippy")} in the bippy!""" // warn
}
