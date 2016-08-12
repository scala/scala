object Test extends App {
  def check[T](body: => T, ok: Boolean): Unit =
    try {
      body
      assert(ok)
    } catch {
      case cce: ClassCastException =>
        assert(!ok)
    }

  val foo: String = "foo"
  assert(foo.isInstanceOf[Singleton])
  assert(!(foo: String).isInstanceOf[Singleton])
  check(foo.asInstanceOf[Singleton], true)
  check((foo: String).asInstanceOf[Singleton], false)

  val bar: "foo" = "foo"
  assert(bar.isInstanceOf[Singleton])
  assert(!(bar: String).isInstanceOf[Singleton])
  check(bar.asInstanceOf[Singleton], true)
  check((bar: String).asInstanceOf[Singleton], false)

  final val baz = "foo"
  assert(baz.isInstanceOf[Singleton])
  assert(!(baz: String).isInstanceOf[Singleton])
  check(baz.asInstanceOf[Singleton], true)
  check((baz: String).asInstanceOf[Singleton], false)

  assert("foo".isInstanceOf[Singleton])
  assert(!("foo": String).isInstanceOf[Singleton])
  check("foo".asInstanceOf[Singleton], true)
  check(("foo": String).asInstanceOf[Singleton], false)
}
