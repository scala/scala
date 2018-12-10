class Implicits {
  final implicit class FooExtender(foo: String)
}

class Test extends Implicits {
  override implicit def FooExtender(foo: String) = super.FooExtender(foo)
}