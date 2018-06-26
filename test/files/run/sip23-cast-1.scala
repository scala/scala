object Test extends App {
  trait Global
  final val global0 = new Global {}
  final val global1 = new Global {}

  // asInstanceOf should do the minimum required to satisfy the verifier, in particular
  // it should not assert equalities.
  // isInstanceOf should check equalities.

  // asInstanceOf
  global0.asInstanceOf[global0.type]
  global0.asInstanceOf[global1.type]

  0.asInstanceOf[0]
  1.asInstanceOf[0]
  null.asInstanceOf[0]
  null.asInstanceOf[1]

  0L.asInstanceOf[0L]
  1L.asInstanceOf[0L]
  null.asInstanceOf[0L]
  null.asInstanceOf[1L]

  0.0.asInstanceOf[0.0]
  1.0.asInstanceOf[0.0]
  null.asInstanceOf[0.0]
  null.asInstanceOf[1.0]

  0.0F.asInstanceOf[0.0F]
  1.0F.asInstanceOf[0.0F]
  null.asInstanceOf[0.0F]
  null.asInstanceOf[1.0F]

  true.asInstanceOf[true]
  true.asInstanceOf[false]
  null.asInstanceOf[true]
  null.asInstanceOf[false]

  'f'.asInstanceOf['f']
  'f'.asInstanceOf['b']
  null.asInstanceOf['f']
  null.asInstanceOf['\u0000']

  // Spec requires null to be an inhabitant of all subtypes of AnyRef,
  // non-null singleton types included.
  "foo".asInstanceOf["foo"]
  "foo".asInstanceOf["bar"]
  null.asInstanceOf["foo"]

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
}
