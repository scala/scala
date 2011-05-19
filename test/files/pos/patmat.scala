// these used to be in test/files/run/patmatnew.scala
// the ticket numbers are from the old tracker, not Trac

object ZipFun {
  //just compilation
  def zipFun[a, b](xs: List[a], ys: List[b]): List[Pair[a, b]] = (Pair(xs, ys): @unchecked) match {
    // !!! case Pair(List(), _), Pair(_, List()) => List()
    case (x :: xs1, y :: ys1) => (x, y) :: zipFun(xs1, ys1)
  }
}

object Test1253 { // compile-only
  def foo(t: (Int, String)) = t match {
    case (1, "") => throw new Exception
    case (r, _) => throw new Exception(r.toString)
  }
}

object Foo1258 {
  case object baz
  def foo(bar: AnyRef) = {
    val Baz = baz
    bar match {
      case Baz => ()
    }
  }
}

object Bug1261 {
  sealed trait Elem
  case class Foo() extends Elem
  case class Bar() extends Elem
  trait Row extends Elem
  object Row {
    def unapply(r: Row) = true

    def f(elem: Elem) {
      elem match {
        case Bar() => ;
        case Row() => ;
        case Foo() => ; // used to give ERROR (unreachable code)
      }
    }
  }
}

sealed abstract class Tree
case class Node(l: Tree, v: Int, r: Tree) extends Tree
case object EmptyTree extends Tree

object Ticket335 { // compile-only
  def runTest() {
    (EmptyTree: Tree @unchecked) match {
      case Node(_, v, _) if (v == 0) => 0
      case EmptyTree => 2
    }
  }
}

object TestIfOpt { //compile-only "test EqualsPatternClass in combination with MixTypes opt, bug #1278"
  trait Token {
    val offset: Int
    def matching: Option[Token]
  }
  def go(tok: Token) = (tok.matching: @unchecked) match {
    case Some(other) if true => Some(other)
    case _ if true => tok.matching match {
      case Some(other) => Some(other)
      case _ => None
    }
  }
}

object Go { // bug #1277 compile-only
  trait Core { def next: Position = null }
  trait Dir
  val NEXT = new Dir {}

  trait Position extends Core

  (null: Core, null: Dir) match {
    case (_, NEXT) if true => false // no matter whether NEXT test succeed, cannot throw column because of guard
    case (at2: Position, dir) => true
  }
}

trait Outer { // bug #1282 compile-only
  object No
  trait File {
    (null: AnyRef) match {
      case No => false
    }
  }
}

class Test806_818 { // #806, #811 compile only -- type of bind
  // bug811
  trait Core {
    trait NodeImpl
    trait OtherImpl extends NodeImpl
    trait DoubleQuoteImpl extends NodeImpl
    def asDQ(node: OtherImpl) = node match {
      case dq: DoubleQuoteImpl => dq
    }
  }

  trait IfElseMatcher {
    type Node <: NodeImpl
    trait NodeImpl
    trait IfImpl
    private def coerceIf(node: Node) = node match {
      case node: IfImpl => node // var node is of type Node with IfImpl!
      case _ => null
    }
  }
}

object Ticket495bis {
  def signum(x: Int): Int =
    x match {
      case 0 => 0
      case _ if x < 0 => -1
      case _ if x > 0 => 1
    }
  def pair_m(x: Int, y: Int) =
    (x, y) match {
      case (_, 0) => 0
      case (-1, _) => -1
      case (_, _) => 1
    }
}

object Ticket522 {
  class Term[X]
  object App {
    // i'm hidden
    case class InternalApply[Y, Z](fun: Y => Z, arg: Y) extends Term[Z]

    def apply[Y, Z](fun: Y => Z, arg: Y): Term[Z] =
      new InternalApply[Y, Z](fun, arg)

    def unapply[X](arg: Term[X]): Option[(Y => Z, Y)] forSome { type Y; type Z } =
      arg match {
        case i: InternalApply[y, z] => Some(i.fun, i.arg)
        case _ => None
      }
  }

  App({ x: Int => x }, 5) match {
    case App(arg, a) =>
  }
}

object Ticket710 {
  def method {
    sealed class Parent()
    case object Child extends Parent()
    val x: Parent = Child
    x match {
      case Child => ()
    }
  }
}
