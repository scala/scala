object Test extends App {
  assert((0: Any) match { case _: 0 => true; case _ => false })
  assert((0: Any) match { case _: 1 => false; case _ => true })
  assert((null: Any) match { case _: 0 => false; case _ => true })
  assert((null: Any) match { case _: 1 => false; case _ => true })

  assert(("foo": Any) match { case _: "foo" => true; case _ => false })
  assert(("foo": Any) match { case _: "bar" => false; case _ => true })
  assert((null: Any) match { case _: "foo" => false; case _ => true })

  object Foo
  object Bar
  assert((Foo: Any) match { case _: Foo.type => true; case _ => false })
  assert((Foo: Any) match { case _: Bar.type => false; case _ => true })
  assert((null: Any) match { case _: Foo.type => false; case _ => true })
}
