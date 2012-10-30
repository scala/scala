trait Foo extends Any { override def equals(x: Any) = false }
trait Ding extends Any { override def hashCode = -1 }

class Bippy1(val x: Int) extends AnyVal with Foo { }  // warn
class Bippy2(val x: Int) extends AnyVal with Ding { } // warn
class Bippy3(val x: Int) extends AnyVal { override def equals(x: Any) = false } // error
class Bippy4(val x: Int) extends AnyVal { override def hashCode = -1 }          // error
case class Bippy5(val x: Int) extends AnyVal { override def productPrefix = "Dingo" } // nothing
case class Bippy6(val x: Int) extends AnyVal { override def productPrefix = "Dingo" ; override def equals(x: Any) = false } // error

