trait Foo1 extends Any
trait Foo2 extends AnyVal
trait Foo3 extends AnyRef

class Bar1 extends Any      // fail
@inline class Bar2 extends AnyVal
class Bar3 extends AnyRef

trait Foo4 extends Any with Immutable // fail
trait Foo5 extends AnyVal with Immutable   // fail
trait Foo6 extends AnyRef with Immutable
