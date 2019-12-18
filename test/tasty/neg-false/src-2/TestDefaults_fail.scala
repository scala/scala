package tastytest

object TestDefaults {

  val defaults = new Defaults()() // error: not enough arguments for constructor
  assert(defaults.i = 33)
  assert(defaults.s = "foo")
  assert(defaults.b = false)

}
