// a.scala
// Thu Sep 27 09:42:16 PDT 2012

trait Bar[-T1, T2, +T3] { }
trait Foo[-T1, T2, +T3] extends Bar[T1, T2, T3]

class A {
  var b = true

  def f1(x: Foo[Int, Int, Int]) = x match {
    /* nowarn */ case _: Foo[Nothing, Int, Any] => true
  }
  def f2[T, U, V](x: Foo[T, U, V]) = x match {
    /* nowarn */ case _: Foo[Nothing, U, Any] => true
  }
  def f3[T, U, V](x: Foo[T, U, V]) = x match {
    /*   warn */ case _: Foo[U, U, V] if b       => ()
    /* nowarn */ case _: Foo[Nothing, U, V] if b => ()
    /*   warn */ case _: Foo[Any, U, V] if b     => ()
  }

  def f4(xs: List[Int]) = xs match {
    /* nowarn - todo */ case x: AnyRef { def bippy: Int } if b => x.bippy  // this could/should do an instance check and not warn
    /* nowarn - todo */ case x: AnyRef { def size: Int } if b  => x.size   // this could/should do a static conformance test and not warn
    /* nowarn */ case x: ((AnyRef { def size: Int }) @unchecked) if b  => x.size
  }
}
