trait Foo { type T }
trait Bar extends Foo { val x : Foo { type T <: Bar.this.T } = this : this.type }
