package s

trait Foo { def to[CC[X]](implicit cc: CC[Int]): Unit }

class Bar extends Foo { def to[CC[X]](implicit cc: CC[Int]): Unit = ??? }
