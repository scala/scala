case class Foo(x: Bar) extends AnyVal
case class Bar(x: Foo) extends AnyVal

class Foo1(val x: Bar1) extends AnyVal
class Bar1(val x: Foo1) extends AnyVal

