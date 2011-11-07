import scala.testing.SUnit._

object Test extends TestConsoleMain {
  def suite = new TestSuite(
    Foo,
    Mas,
    LisSeqArr,
    StreamFoo
  )
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
object Foo extends TestCase("Foo") with Assert {
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
  override def runTest {
    val b = new Bar
    assertEquals(doMatch1(b),(50,"medium"))
    assertEquals(doMatch2(b),null)
    assertEquals(doMatch3(b),"medium")
    assertEquals(doMatch4(b),"medium")
    assertEquals(doMatch5(b),"medium")
    implicit val bc: Int = 3
    assertEquals(4 match {
      case VarFoo(x) => x
    }, 7)
  }
}

// same, but now object is not top-level
object Mas extends TestCase("Mas") with Assert {
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
  def runTest {
    val b = new Baz
    assertEquals(b match {
      case Gaz(s:Int, n:String) => (s,n)
    }, (60,"too large"))
  }
}

object LisSeqArr extends TestCase("LisSeqArr") with Assert {
//  def foo[A](x:List[A]) {}
  def runTest {
    assertEquals((List(1,2,3): Any) match { case   List(x,y,_*) => (x,y)}, (1,2))
    assertEquals((List(1,2,3): Any) match { case    Seq(x,y,_*) => (x,y)}, (1,2))
    //assertEquals((Array(1,2,3): Any) match { case   Seq(x,y,_*) => (x,y)}, (1,2))
    //assertEquals((Array(1,2,3): Any) match { case Array(x,y,_*) => {x,y}}, {1,2})

    // just compile, feature request #1196
//    (List(1,2,3): Any) match { 
//      case a @ List(x,y,_*) => foo(a)
//    }

  }
}


object StreamFoo extends TestCase("unapply for Streams") with Assert {
  //val x:Stream[Int] = Stream.cons(1,x)

  def sum(stream: Stream[Int]): Int =
    stream match {
      case Stream.Empty => 0
      case Stream.cons(hd, tl) => hd + sum(tl)
    }
  override def runTest {
    val str: Stream[Int] = List(1,2,3).toStream
    assertEquals(sum(str), 6)
  }
}

object Test1256 extends TestCase("1256") {
  class Sync {
    def unapply(scrut: Any): Boolean = false
  }
  
  class Buffer {
    val Get = new Sync
    
    val jp: PartialFunction[Any, Any] = {
      case Get() =>
    }
  }
  
  override def runTest { assertFalse((new Buffer).jp.isDefinedAt(42)) }
}
