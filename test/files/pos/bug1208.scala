object Test {
  object Foo
  val f: Option[Foo.type] = Some(Foo)
}
