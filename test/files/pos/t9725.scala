trait Foo1[-X] { def bar[Y <: X](y: Y) = y } // typechecks

// A variant of Foo1 encoding the type parameter Y using a dependent method type.
// error: contravariant type X occurs in covariant position in type  <: X of type Y
trait Foo2[-X] { def bar(x: { type Y <: X })(y: x.Y) = y }

trait Foo3[+X] { def bar[Y >: X](y: Y) = y } // typechecks

// A variant of Foo3 using a dependent method type.
// error: covariant type X occurs in contravariant position in type  >: X of type Y
trait Foo4[+X] { def bar(x: { type Y >: X })(y: x.Y) = y }

// error: covariant type X occurs in contravariant position in type  >: X of type Y
trait Foo9[+X] { def bar(x: { type Y >: X }): Any }
