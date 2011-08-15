object Test {
  def main(args: Array[String]) {
    Foo.run()
    Mas.run()
    LisSeqArr.run()
    StreamFoo.run()
    Test1256.run()
  }
}

// this class is used for representation
class Bar {
  var size: Int    = 50
  var name: String = "medium"
}

// test basic unapply for 0, 1 and 2 args and with precise type test
object Fii {
  def unapply(x: Any): Boolean = x.isInstanceOf[Bar]
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
object VarFoo {
  def unapply(a : Int)(implicit b : Int) : Option[Int] = Some(a + b)
}

object Foo {
  def unapply(x: Any): Option[Product2[Int, String]] = x match {
    case y: Bar => Some(y.size, y.name)
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
  def run() {
    val b = new Bar
    assert(doMatch1(b) == (50,"medium"))
    assert(doMatch2(b) == null)
    assert(doMatch3(b) == "medium")
    assert(doMatch4(b) == "medium")
    assert(doMatch5(b) == "medium")
    implicit val bc: Int = 3
    assert(7 == (4 match {
      case VarFoo(x) => x
    }))
  }
}

// same, but now object is not top-level
object Mas {
  object Gaz {
    def unapply(x: Any): Option[Product2[Int, String]] = x match {
      case y: Baz => Some(y.size, y.name)
      case _ => None
    }
  }
  class Baz {
    var size: Int    = 60
    var name: String = "too large"
  }
  def run() {
    val b = new Baz
    assert((60,"too large") == (b match {
      case Gaz(s:Int, n:String) => (s,n)
    }))
  }
}

object LisSeqArr {
  def run() {
    assert((1,2) == ((List(1,2,3): Any) match { case   List(x,y,_*) => (x,y)}))
    assert((1,2) == ((List(1,2,3): Any) match { case    Seq(x,y,_*) => (x,y)}))
  }
}

object StreamFoo {
  def sum(stream: Stream[Int]): Int =
    stream match {
      case Stream.Empty => 0
      case Stream.cons(hd, tl) => hd + sum(tl)
    }
  def run() {
    val str: Stream[Int] = List(1,2,3).toStream
    assert(6 == sum(str))
  }
}

object Test1256 {
  class Sync {
    def unapply(scrut: Any): Boolean = false
  }

  class Buffer {
    val Get = new Sync
    val jp: PartialFunction[Any, Any] = {
      case Get() =>
    }
  }

  def run() {
    assert(!(new Buffer).jp.isDefinedAt(42))
  }
}
