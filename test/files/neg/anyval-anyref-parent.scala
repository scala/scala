trait Foo1 extends Any
trait Foo2 extends AnyVal // fail
trait Foo3 extends AnyRef

class Bar1 extends Any      // fail
class Bar2(x: Int) extends AnyVal // fail
class Bar3(val x: Int) extends AnyVal // fail
class Bar4 extends AnyRef

trait Foo4 extends Any with Immutable // fail
trait Foo5 extends AnyVal with Immutable   // fail
trait Foo6 extends AnyRef with Immutable
