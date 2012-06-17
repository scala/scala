class Foo (x: AnyRef) {
  def this() = {
    this(new { } ) // okay
  }

  def this(x: Int) = this(new { println(Foo.this)}) // error
}

class Bar (x: AnyRef)
  extends Foo(new { println(Bar.this)}) // error

class Blerg (x: AnyRef) {
  def this() = {
    this(new { class Bar { println(Bar.this); new { println(Bar.this) } }; new Bar } ) // okay
  }
}
