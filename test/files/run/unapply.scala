import scala.testing.SUnit._

object Test {
  def main(args:Array[String]) = {
    Foo.run
    Mas.run
    LisSeqArr.run
    StreamFoo.run
  }
}

// this class is used for representation
class Bar {
  var size: Int    = 50
  var name: String = "medium"
}

// test basic unapply for 0, 1 and 2 args and with precise type test
object Fii {
  def unapply(x: Any): boolean = x.isInstanceOf[Bar]
}
object Faa {
  def unapply(x: Any): Option[String] = if(x.isInstanceOf[Bar]) Some(x.asInstanceOf[Bar].name) else None
}
object FaaPrecise {
  def unapply(x: Bar): Option[String] = Some(x.name)
}
object FaaPreciseSome {
  def unapply(x: Bar) = Some(x.name)  // return type Some[String]
}
object Foo extends Assert {
  def unapply(x: Any): Option[Product2[Int, String]] = x match {
    case y: Bar => Some(Tuple(y.size, y.name))
    case _ => None
  }
  def doMatch1(b:Bar) = b match {
      case Foo(s:Int, n:String) => (s,n)
  }
  def doMatch2(b:Bar) = b match {
      case Fii() => null
  }
  def doMatch3(b:Bar) = b match {
      case Faa(n:String) => n
  }
  def doMatch4(b:Bar) = (b:Any) match {
    case FaaPrecise(n:String) => n
  }
  def doMatch5(b:Bar) = (b:Any) match {
    case FaaPreciseSome(n:String) => n
  }
  def run {
    val b = new Bar
    assertEquals(doMatch1(b),(50,"medium"))
    assertEquals(doMatch2(b),null)
    assertEquals(doMatch3(b),"medium")
    assertEquals(doMatch4(b),"medium")
    assertEquals(doMatch5(b),"medium")
  }
}

// same, but now object is not top-level
object Mas extends Assert {
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
  def run {
    val b = new Baz
    assertEquals(b match {
      case Gaz(s:Int, n:String) => (s,n)
    }, (60,"too large"))
  }
}

object LisSeqArr extends Assert {
  def run {
    assertEquals((List(1,2,3): Any) match { case   List(x,y,_*) => (x,y)}, (1,2))
    assertEquals((List(1,2,3): Any) match { case    Seq(x,y,_*) => (x,y)}, (1,2))
    assertEquals((Array(1,2,3): Any) match { case   Seq(x,y,_*) => (x,y)}, (1,2))
    //assertEquals((Array(1,2,3): Any) match { case Array(x,y,_*) => {x,y}}, {1,2})
  }
}


object StreamFoo extends TestCase("unapply for Streams") with Assert {
  //val x:Stream[Int] = Stream.cons(1,x)

  def sum(stream: Stream[int]): int =
    stream match {
      case Stream.empty => 0
      case Stream.cons(hd, tl) => hd + sum(tl)
    }
  override def run {
    val str: Stream[int] = Stream.fromIterator(List(1,2,3).elements)
    assertEquals(sum(str), 6)
  }
}
