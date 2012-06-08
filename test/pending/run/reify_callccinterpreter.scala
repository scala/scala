import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    type Answer = Value;

    /**
     * A continuation monad.
     */
    case class M[A](in: (A => Answer) => Answer) {
      def bind[B](k: A => M[B])          = M[B](c => in (a => k(a) in c))
      def map[B](f: A => B): M[B]        = bind(x => unitM(f(x)))
      def flatMap[B](f: A => M[B]): M[B] = bind(f)
    }

    def unitM[A](a: A) = M[A](c => c(a))

    def id[A] = (x: A) => x
    def showM(m: M[Value]): String = (m in id).toString()

    def callCC[A](h: (A => M[A]) => M[A]) =
      M[A](c => h(a => M[A](d => c(a))) in c)

    type Name = String

    trait Term
    case class Var(x: Name) extends Term
    case class Con(n: Int) extends Term
    case class Add(l: Term, r: Term) extends Term
    case class Lam(x: Name, body: Term) extends Term
    case class App(fun: Term, arg: Term) extends Term
    case class Ccc(x: Name, t: Term) extends Term

    trait Value
    case object Wrong extends Value {
     override def toString() = "wrong"
    }
    case class Num(n: Int) extends Value {
      override def toString() = n.toString()
    }
    case class Fun(f: Value => M[Value]) extends Value {
      override def toString() = "<function>"
    }

    type Environment = List[Pair[Name, Value]];

    def lookup(x: Name, e: Environment): M[Value] = e match {
      case List() => unitM(Wrong)
      case Pair(y, b) :: e1 => if (x == y) unitM(b) else lookup(x, e1)
    }

    def add(a: Value, b: Value): M[Value] = Pair(a, b) match {
      case Pair(Num(m), Num(n)) => unitM(Num(m + n))
      case _ => unitM(Wrong)
    }

    def apply(a: Value, b: Value): M[Value] = a match {
      case Fun(k) => k(b)
      case _ => unitM(Wrong)
    }

    def interp(t: Term, e: Environment): M[Value] = t match {
      case Var(x) => lookup(x, e)
      case Con(n) => unitM(Num(n))
      case Add(l, r) => for (a <- interp(l, e);
           b <- interp(r, e);
           c <- add(a, b))
                        yield c
      case Lam(x, t) => unitM(Fun(a => interp(t, Pair(x, a) :: e)))
      case App(f, t) => for (a <- interp(f, e);
           b <- interp(t, e);
           c <- apply(a, b))
            yield c
      case Ccc(x, t) => callCC(k => interp(t, Pair(x, Fun(k)) :: e))
    }

    def test(t: Term): String = showM(interp(t, List()))

    val term0 = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
    val term1 = App(Con(1), Con(2))
    val term2 = Add(Con(1), Ccc("k", Add(Con(2), App(Var("k"), Con(4)))))

    println(test(term0))
    println(test(term1))
    println(test(term2))
  }.eval
}
