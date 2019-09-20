trait Foo1 {
  override def toString = "FU"
  trait Bar1 { assert(Foo1.this.toString == "FU") }
  class Bar2 { assert(Foo1.this.toString == "FU") }
}

class Foo2 {
  override def toString = "FU"
  trait Bar1 { assert(Foo2.this.toString == "FU") }
  class Bar2 { assert(Foo2.this.toString == "FU") }
}

trait Baz1 extends Foo1
class Baz2 extends Foo1

trait Baz3 extends Foo2
class Baz4 extends Foo2

object Biz1 extends Baz1 {
  new Bar1 {}
  new Bar2 {}
  new Bar2
}

object Biz2 extends Baz2 {
  new Bar1 {}
  new Bar2 {}
  new Bar2
}

object Biz3 extends Baz3 {
  new Bar1 {}
  new Bar2 {}
  new Bar2
}

object Biz4 extends Baz4 {
  new Bar1 {}
  new Bar2 {}
  new Bar2
}

object Test {
  def main(args: Array[String]): Unit = {
     Biz1
     Biz2
     Biz3
     Biz4
  }
}
