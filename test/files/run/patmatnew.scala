
import scala.language.{ postfixOps }

object Test {

  def main(args: Array[String]) {
    ApplyFromJcl.run()
    Bug1093.run()
    Bug1094.run()
    Bug1270.run()
    Bug1281.run()
    Bug457.run()
    Bug508.run()
    Bug789.run()
    Bug881.run()
    Bug995.run()
    ClassDefInGuard.run()
    SeqUnapply.run()
    SimpleUnapply.run()
    Test1163_Order.run()
    Test717.run()
    Test903.run()
    TestEqualsPatternOpt.run()
    TestGuards.run()
    TestSequence01.run()
    TestSequence02.run()
    TestSequence03.run()
    TestSequence04.run()
    TestSequence05.run()
    TestSequence06.run()
    TestSequence07.run()
    TestSequence08.run()
    TestSimpleIntSwitch.run()
    TestStream.run()
    TestUnbox.run()
    Ticket11.run()
    Ticket2.run()
    Ticket346.run()
    Ticket37.run()
    Ticket44.run()
  }

  def assertEquals(a: Any, b: Any) { assert(a == b) }
  def assertEquals(msg: String, a: Any, b: Any) { assert(a == b, msg) }

  object SimpleUnapply {
    def run() { // from sortedmap, old version
      List((1, 2)).head match {
        case kv@(key, _) => kv.toString + " " + key.toString
      }

    }
  }

  object SeqUnapply {
    case class SFB(i: Int, xs: List[Int])
    def run() {
      List(1, 2) match {
        case List(1) => assert(false, "wrong case")
        case List(1, 2, xs@_*) => assert(xs.isEmpty, "not empty")
        case Nil => assert(false, "wrong case")
      }
      SFB(1, List(1)) match {
        case SFB(_, List(x)) => assert(x == 1)
        case SFB(_, _) => assert(false)
      }
    }
  }

  object ApplyFromJcl {
    def run() {
      val p = (1, 2)
      Some(2) match {
        case Some(p._2) =>
        case _ => assert(false)
      }
    }
  }

  object TestSimpleIntSwitch {
    def run() {
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
      assertEquals("s2boxed", 1, (1: Any) match {
        case 1 => 1
        case _ => 0
      })
      assertEquals("s3", 1, ("hello") match {
        case s: String => 1
        //case _        => 0 // unreachable!
      })
      val xyz: (Int, String, Boolean) = (1, "abc", true);
      assertEquals("s4", 1, xyz._1 match {
        case 1 => 1
        case _ => 0
      })
    }
  }

  // #717 test path of case classes
  object Test717 {
    class Foo(j: Int) {
      case class Bar(i: Int)
    }
    val foo1 = new Foo(1)
    val foo2 = new Foo(2)
    def run() {
      val res = (foo1.Bar(2): Any) match {

        case foo1.Bar(2) => true
      }
      assert(res)
    }
  }

  ///

  trait Treez { self: Shmeez =>
    abstract class Tree
    case class Beez(i: Int) extends Tree
    case object HagbardCeline extends Tree
  }

  trait Shmeez extends AnyRef with Treez {
    val tree: Tree

    def foo = tree match {
      case Beez(2) => 1
      case HagbardCeline => 0
    }
  }

  // multiple guards for same pattern
  object TestGuards extends Shmeez {
    val tree: Tree = Beez(2)
    def run() {
      val res = tree match {
        case Beez(x) if x == 3 => false
        case Beez(x) if x == 2 => true
      }
      assert(res)
      val ret = (Beez(3): Tree) match {
        case Beez(x) if x == 3 => true
        case Beez(x) if x == 2 => false
      }
      assert(ret)
    }
  }

  // test EqualsPatternClass in combination with MixTypes opt, bug #1276
  object TestEqualsPatternOpt {
    val NoContext = new Object
    def run() {
      assertEquals(1, ((NoContext: Any) match {
        case that: AnyRef if this eq that => 0
        case NoContext => 1
        case _ => 2
      }))
    }
  }

  // all ignoring patterns on List
  object TestSequence01 {
    def doMatch(xs: List[String]): String = xs match {
      case List(_*) => "ok"
    }
    def doMatch2(xs: List[String]): List[String] = xs match {
      case List(_, rest@_*) => rest.toList
    }
    def run() {
      val list1 = List()
      assertEquals(doMatch(list1), "ok")
      val list2 = List("1", "2", "3")
      assertEquals(doMatch(list2), "ok")
      val list3 = List("1", "2", "3")
      assertEquals(doMatch2(list3), List("2", "3"))
    }
  }

  // all ignoring patterns on Seq
  object TestSequence02 {
    def doMatch(l: Seq[String]): String = l match {
      case Seq(_*) => "ok"
    }
    def run() {
      val list1 = List()
      assertEquals(doMatch(list1), "ok")
      val list2 = List("1", "2", "3")
      assertEquals(doMatch(list2), "ok")
      val array3 = Array[String]()
      assertEquals(doMatch(array3), "ok")
      val array4 = Array[String]("ga", "gu")
      assertEquals(doMatch(array4), "ok")
    }
  }

  // right-ignoring patterns on List, defaults
  object TestSequence03 {
    def doMatch(xs: List[String]): String = xs match {
      case List(_, _, _, _*) => "ok"
      case _ => "not ok"
    }
    def run() {
      val list1 = List()
      assertEquals(doMatch(list1), "not ok")
      val list2 = List("1", "2", "3")
      assertEquals(doMatch(list2), "ok")
      val list3 = List("1", "2", "3", "4")
      assertEquals(doMatch(list3), "ok")
    }
  }

  // all- and right-ignoring pattern on case class w/ seq param
  object TestSequence04 {
    case class Foo(i: Int, chars: Char*)

    def run() {
      val a = Foo(0, 'a') match {
        case Foo(i, c, chars@_*) => c
        case _ => null
      }
      assertEquals(a, 'a')

      val b = Foo(0, 'a') match {
        case Foo(i, chars@_*) => 'b'
        case _ => null
      }
      assertEquals(b, 'b')
    }
  }

  // sealed case class with ignoring seq patterns
  object TestSequence05 {
    sealed abstract class Con;

    case class Foo() extends Con
    case class Bar(xs: Con*) extends Con

    def run() {
      val res = (Bar(Foo()): Con) match {
        case Bar(xs@_*) => xs // this should be optimized away to a pattern Bar(xs)
        case _ => Nil
      }
      assertEquals("res instance" + res.isInstanceOf[Seq[Con] forSome { type Con }] + " res(0)=" + res(0), true, res.isInstanceOf[Seq[Foo] forSome { type Foo }] && res(0) == Foo())
    }
  }

  // (not regular) fancy guards / bug#644
  object TestSequence06 {

    case class A(i: Any)

    def doMatch(x: Any, bla: Int) = x match {
      case x: A if (bla == 1) => 0
      case A(1) => 1
      case A(A(1)) => 2
    }

    def run() {
      assertEquals(doMatch(A(null), 1), 0)
      assertEquals(doMatch(A(1), 2), 1)
      assertEquals(doMatch(A(A(1)), 2), 2)
    }

  }

  // List of chars
  object TestSequence07 {
    def doMatch1(xs: List[Char]) = xs match {
      case List(x, y, _*) => x :: y :: Nil
    }
    def doMatch2(xs: List[Char]) = xs match {
      case List(x, y, z, w) => List(z, w)
    }
    def doMatch3(xs: Seq[Char]) = xs match {
      case Seq(x, y, 'c', w@_*) => x :: y :: Nil
      case Seq(x, y, z@_*) => z
    }
    def doMatch4(xs: Seq[Char]) = xs match {
      case Seq(x, 'b') => x :: 'b' :: Nil
      case Seq(x, y, z@_*) => z.toList
    }

    def run() {
      assertEquals(List('a', 'b'), doMatch1(List('a', 'b', 'c', 'd')))
      assertEquals(List('c', 'd'), doMatch2(List('a', 'b', 'c', 'd')))
      assertEquals(List('a', 'b'), doMatch3(List('a', 'b', 'c', 'd')))
      assertEquals(List('c', 'd'), doMatch4(List('a', 'b', 'c', 'd')))
    }
  }

  // backquoted identifiers in pattern
  object TestSequence08 {
    def run() {
      val xs = List(2, 3)
      val ys = List(1, 2, 3) match {
        case x :: `xs` => xs
        case _ => Nil
      }
      assertEquals(xs, ys)
    }
  }

  // unapply for Streams
  object TestStream {
    def sum(stream: Stream[Int]): Int =
      stream match {
        case Stream.Empty => 0
        case Stream.cons(hd, tl) => hd + sum(tl)
      }

    val str: Stream[Int] = List(1, 2, 3).iterator.toStream

    def run() { assertEquals(sum(str), 6) }
  }

  // bug#1163 order of temps must be preserved
  object Test1163_Order {
    abstract class Function
    case class Var(n: String) extends Function
    case class Const(v: Double) extends Function

    def f(): (Function, Function) = {
      (Var("x"): Function, Var("y"): Function) match {
        case (Const(v), Const(w)) => throw new Error
        case (leftOne, Var("z")) => throw new Error
        case (leftTwo, rightTwo) => (leftTwo, rightTwo) // was giving "y","x"
      }
    }

    def flips(l: List[Int]): Int = (l: @unchecked) match {
      case 1 :: ls => 0
      case n :: ls => flips((l take n reverse) ::: (l drop n)) + 1
    }

    def run() { assertEquals("both", (Var("x"), Var("y")), f) }
  }

  object TestUnbox {
    def run() {
      val xyz: (Int, String, Boolean) = (1, "abc", true)
      xyz._1 match {
        case 1 => "OK"
        case 2 => assert(false); "KO"
        case 3 => assert(false); "KO"
      }
    }
  }

  object Test903 {
    class Person(_name: String, _father: Person) {
      def name = _name
      def father = _father
    }

    object PersonFather {
      def unapply(p: Person): Option[Person] =
        if (p.father == null)
          None
        else
          Some(p.father)
    }
    def run() {
      val p1 = new Person("p1", null)
      val p2 = new Person("p2", p1)
      assertEquals((p2.name, p1.name), p2 match {
        case aPerson@PersonFather(f) => (aPerson.name, f.name)
        case _ => "No father"
      })
    }
  }

  object Bug881 {
    object Foo1 {
      class Bar1(val x: String)
      def p(b: Bar1) = b.x

      def unapply(s: String): Option[Bar1] =
        Some(new Bar1(s))
    }
    class Foo(j: Int) {
      case class Bar(i: Int)
    }
    def run() {
      "baz" match {
        case Foo1(x) =>
          Foo1.p(x)
      }
    }
  }

  // these are exhaustive matches
  //   should not generate any warnings
  def f[A](z: (Option[A], Option[A])) = z match {
    case (None, Some(x)) => 1
    case (Some(x), None) => 2
    case (Some(x), Some(y)) => 3
    case _ => 4
  }

  def g1[A](z: Option[List[A]]) = z match {
    case Some(Nil) => true
    case Some(x :: Nil) => true
    case _ => true
  }

  def g2[A](z: Option[List[A]]) = z match {
    case Some(x :: Nil) => true
    case Some(_) => false
    case _ => true
  }

  def h[A](x: (Option[A], Option[A])) = x match {
    case (None, _: Some[_]) => 1
    case (_: Some[_], None) => 2
    case (_: Some[_], _: Some[_]) => 3
    case _ => 4
  }

  def j = (List[Int](), List[Int](1)) match {
    case (Nil, _) => 'a'
    case (_, Nil) => 'b'
    case (h1 :: t1, h2 :: t2) => 'c'
  }

  def k(x: AnyRef) = x match {
    case null => 1
    case _ => 2
  }

  val FooBar = 42
  def lala() = 42 match {
    case FooBar => true
  }

  object Bug1270 { // unapply13
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
    def run() {
      assert(!(new Buffer).ps.isDefinedAt(42))
    }
  }

  object Bug1281 {
    class Sync {
      def unapplySeq(scrut: Int): Option[Seq[Int]] = {
        if (scrut == 42) Some(List(1, 2))
        else None
      }
    }
    class Buffer {
      val Get = new Sync
      val jp: PartialFunction[Any, Any] = {
        case Get(xs) => // the argDummy <unapply-selector> should have proper arg.tpe (Int in this case)
      }
    }
    def run() {
      assert(!(new Buffer).jp.isDefinedAt(40))
      assert(!(new Buffer).jp.isDefinedAt(42))
    }
  }

  object ClassDefInGuard {
    val z: PartialFunction[Any, Any] = {
      case x :: xs if xs.forall { y => y.hashCode() > 0 } => 1
    }

    def run() {
      val s: PartialFunction[Any, Any] = {
        case List(4 :: xs) => 1
        case List(5 :: xs) => 1
        case _ if false =>
        case List(3 :: xs) if List(3: Any).forall { g => g.hashCode() > 0 } => 1
      }
      z.isDefinedAt(42)
      s.isDefinedAt(42)
      // just load the thing, to see if the classes are found

      (None: Option[Boolean] @unchecked) match {
        case x if x.map(x => x).isEmpty =>
      }
    }
  }

  // bug#457

  object Bug457 {
    def method1() = {
      val x = "Hello, world"; val y = 100;
      y match {
        case _: Int if (x match { case t => t.trim().length() > 0 }) => false;
        case _ => true;
      }
    }

    def method2(): scala.Boolean = {
      val x: String = "Hello, world"; val y: scala.Int = 100; {
        var temp1: scala.Int = y
        var result: scala.Boolean = false
        if ({
          var result1: scala.Boolean = true;
          if (y == 100)
            result1
          else
            throw new MatchError("crazybox.scala, line 11")
        } && (y > 90))
          result
        else
          throw new MatchError("crazybox.scala, line 9")
      }
    }

    def run() {
      method1();
      method2();
    }
  }

  // bug#508

  object Bug508 {
    case class Operator(x: Int);
    val EQ = new Operator(2);

    def analyze(x: Tuple2[Operator, Int]) = (x: @unchecked) match {
      case (EQ, 0) => "0"
      case (EQ, 1) => "1"
      case (EQ, 2) => "2"
    }
    def run() {
      val x = (EQ, 0);
      assertEquals("0", analyze(x)); // should print "0"
      val y = (EQ, 1);
      assertEquals("1", analyze(y)); // should print "1"
      val z = (EQ, 2);
      assertEquals("2", analyze(z)); // should print "2"
    }
  }

  // bug#789

  object Bug789 { // don't do this at home

    trait Impl

    trait SizeImpl extends Impl { def size = 42 }

    trait ColorImpl extends Impl { def color = "red" }

    type Both = SizeImpl with ColorImpl

    def info(x: Impl) = x match {
      case x: Both => "size  " + x.size + " color " + x.color // you wish
      case x: SizeImpl => "!size " + x.size
      case x: ColorImpl => "color " + x.color
      case _ => "n.a."
    }

    def info2(x: Impl) = x match {
      case x: SizeImpl with ColorImpl => "size  " + x.size + " color " + x.color // you wish
      case x: SizeImpl => "!size " + x.size
      case x: ColorImpl => "color " + x.color
      case _ => "n.a."
    }

    def run() {
      // make up some class that has a size
      class MyNode extends SizeImpl
      assertEquals("!size 42", info(new MyNode))
      assertEquals("!size 42", info2(new MyNode))
    }
  }

  // bug#995

  object Bug995 {
    def foo(v: Any): String = v match {
      case s: Seq[_] => "Seq" // see hack in object Seq.unapplySeq
      case a: AnyRef if runtime.ScalaRunTime.isArray(a) => "Array"
      case _ => v.toString
    }
    def run() { assertEquals("Array", foo(Array(0))) }
  }

  // bug#1093 (contribution #460)

  object Bug1093 {
    def run() {
      assert((Some(3): @unchecked) match {
        case Some(1 | 2) => false
        case Some(3) => true
      })
    }
  }

  // bug#1094 (contribution #461)

  object Bug1094 {
    def foo(ps: String*) = "Foo"
    case class X(p: String, ps: String*)
    def bar =
      X("a", "b") match {
        case X(p, ps@_*) => foo(ps: _*)
      }
    def run() { assertEquals("Foo", bar) }
  }

  // #2

  class Outer_2 {
    case class Foo(x: Int, y: Int) {
      override def equals(other: Any) = other match {
        case Outer_2.this.Foo(`x`, `y`) => true
        case _ => false
      }
    }
  }

  object Ticket2 {
    def run() {
      val o1 = new Outer_2; val o2 = new Outer_2; val x: Any = o1.Foo(1, 2); val y: Any = o2.Foo(1, 2)
      assert(x != y, "equals test returns true (but should not)")
      assert(x match {
        case o2.Foo(x, y) => false
        case o1.Foo(x, y) => true
        case _ => false
      }, "match enters wrong case")
    }
  }

  // #11

  class MyException1 extends Exception

  // Commenting out the following line and uncommenting the second line
  // will cause the test to succeed.
  trait SpecialException extends MyException1
  // trait SpecialException

  class MyException2 extends MyException1 with SpecialException

  object Ticket11 {
    def run() {
      Array[Throwable](new Exception("abc"),
        new MyException1,
        new MyException2).foreach { e =>
          try {
            throw e
          } catch {
            case e: SpecialException => {
              assume(e.isInstanceOf[SpecialException])
            }
            case e => {
              assume(e.isInstanceOf[Throwable])
            }
          }
        }
    }
  }

  // #37

  object Ticket37 {
    def foo() {}
    val (a, b) = { foo(); (2, 3) }
    def run() { assertEquals(this.a, 2) }
  }

  // #44

  trait _X {
    case class _Foo();
    object _Bar {
      def unapply(foo: _Foo): Boolean = true;
    }
  }
  object Y extends _X {
    val foo = _Foo()
    foo match {
      case _Bar() =>
      case _ => assert(false)
    }
  }
  object Ticket44 {
    def run() { assert(Y.toString ne null) /*instantiate Y*/ }
  }

  object Ticket211 {
    def run() {
      (Some(123): Option[Int]) match {
        case (x: Option[a]) if false => {};
        case (y: Option[b]) => {};
      }
    }
  }

  // this test case checks nothing more than whether
  //   case N for object N is translated to a check scrutinee.equals(N)
  //  (or the other way round)... for a long time, we got away with
  //  scrutinee eq N, but those golden days are, apparently, over.
  object Ticket346 {

    class L(val content: List[Int]) {

      def isEmpty = content.isEmpty
      def head = content.head
      def tail = content.tail

      override def equals(that: Any): Boolean = {
        val result = that.isInstanceOf[N.type]
        println("L(" + content + ").equals(" + that + ") returning " + result)
        result
      }
    }

    object N extends L(Nil) {
      override def equals(that: Any): Boolean =
        (that.isInstanceOf[L] && that.asInstanceOf[L].isEmpty)
    }

    object C {

      def unapply(xs: L): Option[(Int, L)] = {
        if (xs.isEmpty) { println("xs is empty"); None }
        else
          Some((xs.head, new L(xs.tail)))
      }

    }

    def empty(xs: L): Boolean = xs match {
      case N => true
      case _ => false
    }

    def singleton(xs: L): Boolean = xs match {
      case C(_, N) => true
      case _ => false
    }

    def run() {
      assert(empty(new L(Nil)))
      assert(singleton(new L(List(1))))
    }

  } // end Ticket346

}
