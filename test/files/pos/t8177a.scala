// exercise coevolveSym
trait Thing { type A; var p: A = _ }
class AA[T](final val x: Thing { type A = T }) {
  def foo: x.A = ???
}

class B extends AA[Int](null) {
  override def foo: B.this.x.A = super.foo
}
