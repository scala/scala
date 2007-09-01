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
      SeqUnapply,
      applyFromJcl,
      new Test717,
      new TestGuards,
      TestEqualsPatternOpt,
      new TestStream,
      new Test903,
      new Test1163_Order,
      new TestUnbox,
      Bug457,
      Bug508,
      Bug789,
      Bug995,
      Bug1093,
      Bug1094,
      ClassDefInGuard,
      Ticket_2
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

  object SeqUnapply extends TestCase("seqUnapply") {
    case class SFB(i:int,xs:List[Int])
    override def runTest() {
      List(1,2) match {
        case List(1) => assert(false, "wrong case")
        case List(1,2,xs @ _*) => assert(xs.isEmpty, "not empty")
        case Nil => assert(false, "wrong case")
      }
      SFB(1,List(1)) match {
        case SFB(_,List(x)) => assert(x==1)
        case SFB(_,_) => assert(false)
      }
    }
  }

  object applyFromJcl extends TestCase("applyFromJcl") {
    override def runTest {
      val p = (1,2)
        Some(2) match {
          case Some(p._2) =>         ;
          case _ => assert(false) ;
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

  object TestEqualsPatternOpt extends TestCase("test EqualsPatternClass in combination with MixTypes opt, bug #1276") {
    val NoContext = new Object
    override def runTest {
      assertEquals(1,((NoContext:Any) match {
        case that : AnyRef if this eq that => 0
        case NoContext => 1
        case _ => 2
      }))
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

  class TestUnbox extends TestCase("unbox") {
    override def runTest() {
      val xyz: (int, String, boolean) = (1, "abc", true)
      xyz._1 match {
        case 1 => "OK"
        case 2 => assert(false); "KO"
        case 3 => assert(false); "KO"
      }
    }
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


  object Test1253  { // compile-only
    def foo(t : (Int, String)) = t match {
      case (1, "") => throw new Exception
      case (r, _) => throw new Exception(r.toString)
    }
  }

  object Foo1258 {
    case object baz
    def foo(bar : AnyRef) = {
      val Baz = baz
      bar match {
        case Baz => ()
      }
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

  object Bug1270  { // unapply13

    class Sync {
      def apply(x: Int): Int = 42
      def unapply(scrut: Any): Option[Int] = None
    }

    class Buffer {
      object Get extends Sync

      var ps: PartialFunction[Any, Any] = {
        case Get(y) if y > 4 => // y gets a wildcard type for some reason?! hack
      }
    }

    println((new Buffer).ps.isDefinedAt(42))
  }

  object Bug1261 {
    sealed trait Elem
    case class Foo extends Elem
    case class Bar extends Elem
    trait Row extends Elem
    object Row {
      def unapply(r: Row) = true

      def f(elem: Elem) {
        elem match {
          case Bar() => ;
          case Row() => ;
          case Foo() => ; // used to give ERROR (unreachable code)
        }}}
  }
/*
  object Feature1196 {
    def f(l: List[Int]) { }

    val l: Seq[Int] = List(1, 2, 3)

    l match {
      case x @ List(1, _) => f(x) // x needs to get better type List[int] here
    }
  }
*/
  object TestIfOpt { //compile-only "test EqualsPatternClass in combination with MixTypes opt, bug #1278"
     trait Token {
       val offset : Int
       def matching : Option[Token]
     }
    def go(tok : Token) = tok.matching match {
      case Some(other) if true => Some(other)
      case _ if true => tok.matching match {
        case Some(other) => Some(other)
        case _ => None
      }
    }
  }

  object Go { // bug #1277 compile-only
    trait Core { def next : Position = null }
    trait Dir
    val NEXT = new Dir{}

    trait Position extends Core

    (null:Core,null:Dir) match {
      case (_, NEXT) if true => false // no matter whether NEXT test succeed, cannot throw column because of guard
      case (at2:Position,dir) => true
    }
  }

  trait Outer { // bug #1282 compile-only
    object No
    trait File {
      (null:AnyRef) match {
        case No => false
      }
    }
  }

  object cast2 { // #1281

    class Sync {
      def unapplySeq(scrut: Int): Option[Seq[Int]] = {
        println("unapplySeq: "+scrut)
        if (scrut == 42) Some(List(1, 2))
        else None
      }
    }

    class Buffer {
      val Get = new Sync

      val jp: PartialFunction[Any, Any] = {
        case Get(xs) => println(xs) // the argDummy <unapply-selector> should have proper arg.tpe (Int in this case)
      }
    }

    println((new Buffer).jp.isDefinedAt(40))
    println((new Buffer).jp.isDefinedAt(42))
  }

  object ClassDefInGuard extends TestCase("classdef in guard") { // compile-and-load only
    val z:PartialFunction[Any,Any] = {
      case x::xs if xs.forall { y => y.hashCode() > 0 } => 1
    }

    override def runTest {
    val s:PartialFunction[Any,Any] = {
      case List(4::xs)                => 1
      case List(5::xs)                => 1
      case _                if false                               =>
      case List(3::xs) if List(3:Any).forall { g => g.hashCode() > 0 } => 1
    }
      z.isDefinedAt(42)
      s.isDefinedAt(42)
      // just load the thing, to see if the classes are found

      (None:Option[Boolean]) match {
        case x if x.map(x => x).isEmpty =>
      }
    }
  }

  // bug#457

  object Bug457 extends TestCase("Bug457") {
    def method1() = {
      val x = "Hello, world"; val y = 100;
      y match {
        case _: Int if (x match { case t => t.trim().length() > 0 }) => false;
        case _ => true;
      }}

    def method2(): scala.Boolean = {
      val x: String = "Hello, world"; val y: scala.Int = 100; {
        var temp1: scala.Int = y;
        var result: scala.Boolean = false;
        if (
          {
            var result1: scala.Boolean = true;
            if (y == 100)
              result1
            else
              throw new MatchError("crazybox.scala, line 11")
          } && (y > 90)
        )
          result
            else
              throw new MatchError("crazybox.scala, line 9")
      }}

    override def runTest {
      method1();
      method2();
    }
  }

  // bug#508

  object Bug508 extends TestCase("aladdin #508") {
    case class Operator(x: Int);
    val EQ = new Operator(2);

    def analyze(x: Pair[Operator, Int]) = x match {
      case Pair(EQ, 0) => "0"
      case Pair(EQ, 1) => "1"
      case Pair(EQ, 2) => "2"
    }
    override def runTest {
      val x = Pair(EQ, 0);
      assertEquals("0", analyze(x)); // should print "0"
      val y = Pair(EQ, 1);
      assertEquals("1", analyze(y)); // should print "1"
      val z = Pair(EQ, 2);
      assertEquals("2", analyze(z)); // should print "2"
    }
  }

  // bug#789

  object Bug789 extends TestCase("aladdin #789") { // don't do this at home

    trait Impl

    trait SizeImpl extends Impl { def size = 42 }

    trait ColorImpl extends Impl { def color = "red" }

    type Both = SizeImpl with ColorImpl

    def info(x:Impl) = x match {
      case x:Both      => "size  "+x.size+" color "+x.color // you wish
      case x:SizeImpl  => "!size "+x.size
      case x:ColorImpl => "color "+x.color
      case _           => "n.a."
    }

    def info2(x:Impl) = x match {
      case x:SizeImpl with ColorImpl  => "size  "+x.size+" color "+x.color // you wish
      case x:SizeImpl  => "!size "+x.size
      case x:ColorImpl => "color "+x.color
      case _           => "n.a."
    }

    override def runTest {
      // make up some class that has a size
      class MyNode extends SizeImpl
      assertEquals("!size 42", info(new MyNode))
      assertEquals("!size 42", info2(new MyNode))
    }
  }

  // bug#995

  object Bug995 extends TestCase("aladdin #995") {
    def foo(v: Any): String = v match {
      case s: Seq[_] => "Seq" // see hack in object Seq.unapplySeq
      //case a: AnyRef if runtime.ScalaRunTime.isArray(a) => "Array"
      case _ => v.toString
    }
    override def runTest { assertEquals("Seq", foo(Array(0))) }
  }

  // bug#1093 (contribution #460)

  object Bug1093 extends TestCase("aladdin #1093") {
    override def runTest {assertTrue(Some(3) match {
      case Some(1 | 2) => false
      case Some(3) => true
    })}
  }

  // bug#1094 (contribution #461)

  object Bug1094 extends TestCase("aladdin #1094") {
    def foo(ps: String*) = "Foo"
    case class X(p: String, ps: String*)
    def bar =
      X("a", "b") match {
        case X(p, ps @ _*) => foo(ps : _*)
      }
    override def runTest { assertEquals("Foo", bar) }
  }

  // #2

  class Outer_2 {
    case class Foo(x: int, y: int) {
      override def equals(other: Any) = other match {
        case Outer_2.this.Foo(`x`, `y`) => true
        case _ => false
      }
    }
  }

  object Ticket_2 extends TestCase("#2") { override def runTest {
    val o1 = new Outer_2; val o2 = new Outer_2; val x: Any = o1.Foo(1, 2); val y: Any = o2.Foo(1, 2)
    assertFalse("equals test returns true (but should not)", x equals y)
    assertTrue("match enters wrong case", x match {
      case o2.Foo(x, y) => false;
      case o1.Foo(x, y) => true
      case _ => false
    })
  }}

}


