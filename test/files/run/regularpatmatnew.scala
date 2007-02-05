object Test {
  import scala.testing.SUnit._

  def main(args:Array[String]): Unit = {
    val tr = new TestResult
    new TestSuite(

      new Test01,
      new Test02,
      new Test03,
      new Test04,
      new Test05,
      new Test06,
      new Test07

    ).run(tr)

    for(val f <- tr.failures())
      Console println f
  }

  class Test01 extends TestCase("uno (all ignoring patterns on List)") {
    def doMatch(l:List[String]):String = l match {
        case List(_*) => "ok"
    }
    override def runTest() = {
      val list1 = List();
      assertEquals(doMatch(list1), "ok");
      val list2 = List("1","2","3");
      assertEquals(doMatch(list2), "ok");
    }
  }

  /* these are not allowed, for strange reasons I will never figure out

    def doMatch(l:Seq[String]):String = l match {
        case List(_*) => "ok"
        case _ => "not ok"
    }

    def doMatch(l:Seq[String]):String = l match {
        case Array(_*) => "ok"
        case _ => "not ok"
    }
    */

  class Test02 extends TestCase("due (all ignoring patterns on Seq)") {
    def doMatch(l:Seq[String]):String = l match {
        case Seq(_*) => "ok"
    }
    override def runTest() = {
      val list1 = List();
      assertEquals(doMatch(list1), "ok");
      val list2 = List("1","2","3");
      assertEquals(doMatch(list2), "ok");
      val array3 = Array[String]();
      assertEquals(doMatch(array3), "ok");
      val array4 = Array[String]("ga","gu");
      assertEquals(doMatch(array4), "ok");
    }
  }

  class Test03 extends TestCase("tre (right-ignoring patterns on List, defaults)") {
    def doMatch(l:List[String]):String = l match {
        case List(_,_,_,_*) => "ok"
        case _ => "not ok"
    }
    override def runTest() = {
      val list1 = List();
      assertEquals(doMatch(list1), "not ok");
      val list2 = List("1","2","3");
      assertEquals(doMatch(list2), "ok");
      val list3 = List("1","2","3","4");
      assertEquals(doMatch(list3), "ok");
    }
  }


  class Test04 extends TestCase("quattro (all- and right-ignoring pattern on case class w/ seq param)") {
    case class Foo(i: Int, chars: Char*)

    override def runTest() = {
      val a = Foo(0, 'a') match {
        case Foo(i, c, chars @ _*) =>
          c
        case _ =>
          null
      }
      assertEquals(a,'a')

      val b = Foo(0, 'a') match {
        case Foo(i, chars @ _*) =>
          'b'
        case _ =>
          null
      }
      assertEquals(b,'b')
    }
  }

  class Test05 extends TestCase("cinque (sealed case class with ignoring seq patterns)") {
    sealed abstract class Con;

    case class Foo() extends Con
    case class Bar(xs:Con*) extends Con

    override def runTest() = {
      val res = (Bar(Foo()):Con) match {
        case Bar(xs@_*) => xs // this should be optimized away to a pattern Bar(xs)
        case _ => Nil
      }
      assertEquals("res instance"+res.isInstanceOf[Seq[Con]]+" res(0)="+res(0), true, res.isInstanceOf[Seq[Foo]] && res(0) == Foo() )
    }
  }

  class Test06 extends TestCase("sei (not regular) fancy guards / bug#644 ") {

    case class A(i:Any)

    def doMatch(x:Any, bla:int) = x match {
      case x:A if (bla==1) =>
        0
      case A(1) =>
        1
      case A(A(1)) =>
        2
    }

    override def runTest(): Unit= {
      assertEquals(doMatch(A(null),1), 0)
      assertEquals(doMatch(A(1),2), 1)
      assertEquals(doMatch(A(A(1)),2), 2)
    }

  }

  class Test07 extends TestCase("sette List of chars") {
    def doMatch1(xs:List[char]) = xs match {
      case List(x, y, _*) => x::y::Nil
    }
    def doMatch2(xs:List[char]) = xs match {
      case List(x, y, z, w) => List(z,w)
    }
    //def doMatch3(xs:List[char]) = xs match {
    //  case List(_*, z, w) => w::Nil
    //}
    override def runTest() {
      assertEquals(doMatch1(List('a','b','c','d')), List('a','b'))
      assertEquals(doMatch2(List('a','b','c','d')), List('c','d'))
      //assertEquals(doMatch3(List('a','b','c','d')), List('d'))
    }
  }
}
