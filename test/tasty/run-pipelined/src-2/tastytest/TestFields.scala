package tastytest

import lib.Fields

object TestFields extends scala.App {
  val foo: "foo" = Fields.FOO // Testing reading a final static field.
  val bar: String = Fields.BAR // Testing reading a static field.

  val fields = new Fields

  val baz: String = fields.baz // Testing reading a final field.
  val qux: String = fields.qux // Testing reading a field.

  assert(foo == "foo")
  assert(bar == "bar")
  assert(baz == "baz")
  assert(qux == "qux")
}
