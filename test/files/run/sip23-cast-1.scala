object Test extends App {
  def check[T](body: => T, ok: Boolean): Unit =
    try {
      body
      assert(ok)
    } catch {
      case cce: ClassCastException =>
        assert(!ok)
    }

  trait Global
  final val global0 = new Global {}
  final val global1 = new Global {}

  // asInstanceOf
  check(global0.asInstanceOf[global0.type], true)
  check(global0.asInstanceOf[global1.type], false)

  check(0.asInstanceOf[0], true)
  check(1.asInstanceOf[0], false)
  check(null.asInstanceOf[0], false)
  check(null.asInstanceOf[1], false)

  check(0L.asInstanceOf[0L], true)
  check(1L.asInstanceOf[0L], false)
  check(null.asInstanceOf[0L], false)
  check(null.asInstanceOf[1L], false)

  check(0.0.asInstanceOf[0.0], true)
  check(1.0.asInstanceOf[0.0], false)
  check(null.asInstanceOf[0.0], false)
  check(null.asInstanceOf[1.0], false)

  check(0.0F.asInstanceOf[0.0F], true)
  check(1.0F.asInstanceOf[0.0F], false)
  check(null.asInstanceOf[0.0F], false)
  check(null.asInstanceOf[1.0F], false)

  check(true.asInstanceOf[true], true)
  check(true.asInstanceOf[false], false)
  check(null.asInstanceOf[true], false)
  check(null.asInstanceOf[false], false)

  check('f'.asInstanceOf['f'], true)
  check('f'.asInstanceOf['b'], false)
  check(null.asInstanceOf['f'], false)
  check(null.asInstanceOf['\u0000'], false)

  // Spec requires null to be an inhabitant of all subtypes of AnyRef,
  // non-null singleton types included.
  check("foo".asInstanceOf["foo"], true)
  check("foo".asInstanceOf["bar"], false)
  check(null.asInstanceOf["foo"], true)

  check('foo.asInstanceOf['foo], true)
  check('foo.asInstanceOf['bar], false)
  check(null.asInstanceOf['foo], true)

  // isInstanceOf
  assert(global0.isInstanceOf[global0.type])
  assert(!global0.isInstanceOf[global1.type])

  assert(0.isInstanceOf[0])
  assert(!1.isInstanceOf[0])
  assert(!null.isInstanceOf[0])
  assert(!null.isInstanceOf[1])

  assert(0L.isInstanceOf[0L])
  assert(!1L.isInstanceOf[0L])
  assert(!null.isInstanceOf[0L])
  assert(!null.isInstanceOf[1L])

  assert(0.0.isInstanceOf[0.0])
  assert(!1.0.isInstanceOf[0.0])
  assert(!null.isInstanceOf[0.0])
  assert(!null.isInstanceOf[1.0])

  assert(0.0F.isInstanceOf[0.0F])
  assert(!1.0F.isInstanceOf[0.0F])
  assert(!null.isInstanceOf[0.0F])
  assert(!null.isInstanceOf[1.0F])

  assert(true.isInstanceOf[true])
  assert(!true.isInstanceOf[false])
  assert(!null.isInstanceOf[true])
  assert(!null.isInstanceOf[false])

  assert('f'.isInstanceOf['f'])
  assert(!'f'.isInstanceOf['b'])
  assert(!null.isInstanceOf['f'])
  assert(!null.isInstanceOf['\u0000'])

  // Despite the spec the implementation doesn't treat null as an
  // inhabitant of subtypes of AnyRef when doing an isInstanceOf
  // test.
  assert("foo".isInstanceOf["foo"])
  assert(!"foo".isInstanceOf["bar"])
  assert(!null.isInstanceOf["foo"])

  assert('foo.isInstanceOf['foo])
  assert(!'foo.isInstanceOf['bar])
  assert(!null.isInstanceOf['foo])
}
