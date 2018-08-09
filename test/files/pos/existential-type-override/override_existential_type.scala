class Foo[T]

class DerivedFoo[T] extends Foo[T]

class Bar(val foo: Foo[_])

class DerivedBar(override val foo: DerivedFoo[_]) extends Bar(foo)

class OtherDerivedFoo[T, U] extends Foo[T]

//when with multiple existential type, also should compile with override
class OtherDerivedBar(override val foo: OtherDerivedFoo[_,  _]) extends Bar(foo)
