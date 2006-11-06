object Test {
  def main(args:Array[String]) = {
    Foo.run
    Mas.run
  }
}
object Foo {
  def unapply(x: Any): Option[Product2[Int, String]] = x match {
    case y: Bar => Some(Tuple(y.size, y.name))
    case _ => None
  }/*
  // ERROR: test/pending/pos/unapply.scala:6 error: method unapply is defined twice
  def unapply(x: Any): Option[Product1[String]] = x match {
    case y: Bar => Some(Tuple(y.name))
    case _ => None
  }*/
  def run: Unit = {
    val b = new Bar
    b match {
      case Foo(s:Int, n:String) => Console.println("size "+s+" name "+n)
    }
    b.size = 54
    b.name = "large"
    b match {
      case Foo(s:Int, n:String) => Console.println("size "+s+" name "+n)
    }/*
    b match {
      case Foo(n) => Console.println("name " + n)
    }*/
  }
}

class Bar {
  var size: Int    = 50
  var name: String = "medium"
}

object Mas {
  object Gaz {
    def unapply(x: Any): Option[Product2[Int, String]] = x match {
      case y: Baz => Some(Tuple(y.size, y.name))
      case _ => None
    }
  }
  class Baz {
    var size: Int    = 60
    var name: String = "too large"
  }
  def run: Unit = {
    val b = new Baz
    b match {
      case Gaz(s:Int, n:String) => Console.println("size "+s+" name "+n)
    }
  }
}
