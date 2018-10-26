// document status quo (used to crash, now just fails to infer)
// to make this compile, explicitly provide type args
class C[T](x: T)
class Foo[F[_], G[_]](value: F[C[G[Foo[F, G]]]])

class Test {
  type Id[A] = A
  new Foo(Some(new C(new Foo[Option, Id](None))))
}