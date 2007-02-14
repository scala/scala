trait Treez requires Shmeez {
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

object Test {
  import scala.testing.SUnit._

  def main(args:Array[String]): Unit = {
    val tr = new TestResult
    new TestSuite(
      new TestSimpleIntSwitch,
      new Test717,
      new TestGuards
    ).run(tr)

    for(val f <- tr.failures())
      Console println f
  }

  class Foo(j:Int) {
    case class Bar(i:Int)
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
}

