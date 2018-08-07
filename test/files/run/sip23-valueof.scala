object Test extends App {
  object Foo
  val foo = "foo"

  implicitly[ValueOf[1]]
  implicitly[ValueOf[1L]]
  implicitly[ValueOf[1.0]]
  implicitly[ValueOf[1.0F]]
  implicitly[ValueOf[true]]
  implicitly[ValueOf['f']]
  implicitly[ValueOf["foo"]]
  implicitly[ValueOf[Unit]]
  implicitly[ValueOf[Foo.type]]
  implicitly[ValueOf[foo.type]]

  assert((valueOf[1]: 1) == 1)
  assert((valueOf[1L]: 1L) == 1L)
  assert((valueOf[1.0]: 1.0) == 1.0)
  assert((valueOf[1.0F]: 1.0F) == 1.0F)
  assert((valueOf[true]: true) == true)
  assert((valueOf['f']: 'f') == 'f')
  assert((valueOf["foo"]: "foo") == "foo")
  assert((valueOf[Unit]: Unit) == ((): Any))
  assert((valueOf[Foo.type]: Foo.type) eq Foo)
  assert((valueOf[foo.type]: foo.type) eq foo)
}
