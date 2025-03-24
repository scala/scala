package test

/* check that members are visible when completing inside an Apply with insufficient args */

class Foo {
  def Bar: Double = 2.0
  def Baz: Double = 1.0
}

class Completion1 {
  def singleArg(f: Foo => Double): Nothing = ???
  singleArg(_./*!*/)

  def multiArg(f: Foo => Double, s: String): Nothing = ???
  multiArg(_./*!*/) // importantly we want to see Bar and Baz as completions here

  def multiArgFull(f: Foo => Double, s: String): Nothing = ???
  multiArgFull(_./*!*/,???)

  def multiArgWithDefault(f: Foo => Double, s: String = "hello"): Nothing = ???
  multiArgWithDefault(_./*!*/)
}
