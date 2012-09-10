package scala.t6126

@deprecatedInheritance("`Foo` will be made final in a future version.", "2.10.0")
class Foo

class SubFoo extends Foo

@deprecatedInheritance()
trait T

object SubT extends T

@deprecatedInheritance()
trait S

object O {
  new S {
  }
}
