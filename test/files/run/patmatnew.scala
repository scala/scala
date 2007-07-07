trait Treez { self: Shmeez =>
  abstract class Tree
  case class Beez(i:Int) extends Tree
  case object HagbardCeline extends Tree
}

trait Shmeez extends AnyRef with Treez {
  val tree: Tree

  def foo = tree match {
    case Beez(2) => 1
    case HagbardCeline => 0
  }
}

import scala.testing.SUnit._

object Test extends TestConsoleMain {

  //just compilation
  def zipFun[a,b](xs:List[a], ys:List[b]):List[Pair[a,b]] = (Pair(xs,ys): @unchecked) match {
    // !!! case Pair(List(), _), Pair(_, List()) => List()
    case Pair(x :: xs1, y :: ys1) => Pair(x, y) :: zipFun(xs1, ys1)
  }

  def suite = new TestSuite(
      new TestSimpleIntSwitch,
      new SimpleUnapply,
      new Test717,
      new TestGuards,
      new TestStream,
      new Test903,
      new Test1093,
      new Test1163_Order
    )

  class Foo(j:Int) {
    case class Bar(i:Int)
  }
  class SimpleUnapply extends TestCase("simpleUnapply") {
    override def runTest() { // from sortedmap, old version
      List((1,2)).head match {
        case kv @ Pair(key, _) => kv.toString + " " + key.toString
      }
    }
  }
  class TestSimpleIntSwitch extends TestCase("SimpleIntSwitch") {
    override def runTest() = {
      assertEquals("s1", 1, 1 match {
        case 3 => 3
        case 2 => 2
        case 1 => 1
        case 0 => 0
      })
      assertEquals("s2", 1, 1 match {
        case 1 => 1
        case _ => 0
      })
      assertEquals("s2boxed", 1, (1:Any) match {
        case 1 => 1
        case _ => 0
      })
      assertEquals("s3", 1, ("hello") match {
        case s:String => 1
        case _        => 0
      })
      val xyz: (int, String, boolean) = (1, "abc", true);
      assertEquals("s4", 1, xyz._1 match {
        case 1 => 1
        case _ => 0
      })
    }
  }
  class Test717 extends TestCase("#717 test path of case classes") {
    val foo1 = new Foo(1)
    val foo2 = new Foo(2)

    override def runTest() = {
      val res = (foo1.Bar(2):Any) match {
       case foo2.Bar(2) => false
       case foo1.Bar(2) => true
      }
      assertTrue("ok", res);
    }
  }

  class TestGuards extends TestCase("multiple guards for same pattern") with Shmeez {
    val tree:Tree = Beez(2)
    override def runTest = {
      val res = tree match {
        case Beez(x) if x == 3 => false
        case Beez(x) if x == 2 => true
      }
      assertTrue("ok", res);
      val ret = (Beez(3):Tree) match {
        case Beez(x) if x == 3 => true
        case Beez(x) if x == 2 => false
      }
      assertTrue("ok", ret);
    }
  }

  class TestStream extends TestCase("unapply for Streams") {
    def sum(stream: Stream[int]): int =
      stream match {
        case Stream.empty => 0
        case Stream.cons(hd, tl) => hd + sum(tl)
      }

    val str: Stream[int] = Stream.fromIterator(List(1,2,3).elements)

    def runTest() = assertEquals(sum(str), 6)
  }

  class Test1163_Order extends TestCase("bug#1163 order of temps must be preserved") {
    abstract class Function
    case class Var(n: String) extends Function
    case class Const(v: double) extends Function

    def f(): (Function, Function) = {
      (Var("x"): Function, Var("y"): Function) match {
        case (Const(v), Const(w)) => throw new Error
        case (leftOne, Var("z")) => throw new Error
        case (leftTwo, rightTwo) => (leftTwo, rightTwo) // was giving "y","x"
      }
    }

    def flips(l: List[int]): int = (l: @unchecked) match {
      case 1 :: ls => 0
      case n :: ls => flips((l take n reverse) ::: (l drop n)) + 1
    }

    def runTest() =  assertEquals("both", (Var("x"),Var("y")), f)
  }

  class Test806_818 { // #806, #811 compile only -- type of bind
    // bug811
    trait Core {
      trait NodeImpl;
      trait OtherImpl extends NodeImpl;
      trait DoubleQuoteImpl extends NodeImpl;
      def asDQ(node : OtherImpl) = node match {
	case dq : DoubleQuoteImpl => dq;
      }
    }

    trait IfElseMatcher {
      type Node <: NodeImpl;
      trait NodeImpl;
      trait IfImpl;
      private def coerceIf(node : Node) = node match {
        case node : IfImpl => node; // var node is of type Node with IfImpl!
        case _ => null;
      }
    }
  }


  class Person(_name : String, _father : Person) {
    def name = _name
    def father = _father
  }

  object PersonFather {
    def unapply(p : Person) : Option[Person]  =
      if (p.father == null)
        None
      else
  	    Some(p.father)
  }

  class Test1093 extends TestCase("bug1093") {
    override def runTest {
      val x = Some(3) match {
        case Some(1 | 2) => 1
        case Some(3) => 2
      }
      assertEquals("ok", 2, x)
    }
  }

  class Test903 extends TestCase("bug903") {

  override def runTest = {
    val p1 = new Person("p1",null)
    val p2 = new Person("p2",p1)
    assertEquals((p2.name, p1.name), p2 match {
      case aPerson@PersonFather(f) => (aPerson.name,f.name)
      case _ => "No father"
    })
  }
  }


  object Foo1 {
    class Bar1(val x : String)
    def p(b : Bar1) = Console.println(b.x)

    def unapply(s : String) : Option[Bar1] =
      Some(new Bar1(s))
  }

  object bug881 extends TestCase("881") {
    override def runTest = {
      "baz" match {
        case Foo1(x) =>
          Foo1.p(x)
      }
    }
  }


  // these are exhaustive matches
  //   should not generate any warnings
  def f[A](z:(Option[A],Option[A])) = z match {
    case Pair(None,Some(x)) => 1
    case Pair(Some(x),None ) => 2
    case Pair(Some(x),Some(y)) => 3
    case _ => 4
  }

  def g1[A](z:Option[List[A]]) = z match {
    case Some(Nil) => true
    case Some(x::Nil) => true
    case _ => true
  }

  def g2[A](z:Option[List[A]]) = z match {
    case Some(x::Nil) => true
    case Some(_) => false
    case _ => true
  }

  def h[A](x: (Option[A],Option[A])) = x match {
    case Pair(None,_:Some[_]) => 1
    case Pair(_:Some[_],None ) => 2
    case Pair(_:Some[_],_:Some[_]) => 3
    case _ => 4
  }

  def i = List(1,2) match {
    case List(1) =>
    case List(1,2,xs @ _*) =>
    case Nil =>
  }

  def j = (List[Int](), List[Int](1)) match {
    case (Nil, _) => 'a'
    case (_, Nil) => 'b'
    case (h1 :: t1, h2 :: t2) => 'c'
  }

  def k (x:AnyRef) = x match {
    case null => 1
    case _ => 2
  }

  val FooBar = 42
  def lala() = 42 match {
    case FooBar => true
  }

}

