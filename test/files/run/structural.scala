
import scala.language.{ reflectiveCalls }

object test1 {

  val o1 = new Object { override def toString = "ohone" }
  val o2 = new Object { override def toString = "ohtwo" }

  val t1 = new Tata("tieone")
  val t2 = new Tata("tietwo")

  class Tata(name: String) {
    override def toString = name
    def tatMe = "oy"
  }

  class Titi extends Tata("titi")

  object Rec {
    val a = 1
    val b = 2
    val c = "hey"
    def d(x: AnyRef) = new Object { override def toString = "dee" }
    def e(x: Tata) = new Tata("iei")
    def f(x: Int) = x + 1
    def g(x: Int) = { v = x }
    def h(x: Unit) = new Object { override def toString = "eitch" }
    def i(x: Array[Int]) = x(0)
    def j(x: Array[AnyRef]) = x(0)
    def k(x: Array[Char]) = x(0)
    def l(x: Array[Unit]) = x(0)
    def m(x: Array[String]) = x(0)
    def n(x: Array[Tata]) = x(0)
    def o: Array[Int] = Array(1, 2, 3)
    def p: Array[AnyRef] = Array(o1, o2)
    def q: Array[Char] = Array('1', '2')
    def r: Array[Unit] = Array((), ())
    def s: Array[String] = Array("one", "two")
    def t: Array[Tata] = Array(t1, t2)
    def u[T](f: T=>T, v:T): T = f(v)
    var v = 4
    var w = 11
    val x = t1
    val y: Tata = null
    def z(t: Tata) = ()
  }

  type rt = Object {
    val a: Int;
    val c: String;
    def d(x: AnyRef): AnyRef
    def e(x: Tata): Tata
    def f(x: Int): Int;
    def h(x: Unit): AnyRef;
    def i(x: Array[Int]): Int
    def j(x: Array[AnyRef]): AnyRef
    def k(x: Array[Char]): Char
    def l(x: Array[Unit]): Unit
    def m(x: Array[String]): String
    def n(x: Array[Tata]): Tata
    def o: Array[Int]
    def p: Array[AnyRef]
    def q: Array[Char]
    def r: Array[Unit]
    def s: Array[String]
    def t: Array[Tata]
    def u[T](f: T=>T, v:T): T
    var v: Int
    val y: Tata
  }

  def l (r: rt) {
    println(" 1. " + r.c)
    println(" 2. " + r.a + 1)
    println(" 3. " + r.d(o1))
    println(" 4. " + r.e(t1))
    println(" 5. " + (r.f(4) + 1))
    println(" 6. " + r.f(4) + 1)
    println(" 7. " + r.f(r.a))
    println(" 8. " + r.v)
    r.v = r.v + 1
    println("10. " + r.v)
    println("11. " + r.h(()))
    println("12. " + r.i(Array(1, 2, 3)))
    println("13. " + r.j(Array(o1, o2)))
    println("14. " + r.k(Array('1', '2')))
    println("15. " + r.l(Array((), ())))
    println("16. " + r.m(Array("one", "two")))
    println("17. " + r.n(Array(t1, t2)))
    println("18. " + (r.o(0) + 1))
    println("19. " + (r.p(0).hashCode() > 0))
    println("20. " + r.q(0))
    println("21. " + r.r(0))
    println("22. " + r.m(r.s))
    println("23. " + r.t(0).tatMe)
    println("24. " + r.u[Int](_+1,0))
    println("25. " + r.y)
    println("26. " + r.e(null))
  }

  /*def ma[T](r: Object{def e(x: T): T; val x: T}) {
    println("30. " + r.e(r.x)) // static error
  }*/

  def mb(r: Object { def e[T](x: T): T }) {
    println("31. " + r.e[Int](4)) // while this is ok
  }

  def m1(r: Object { def z(x: Tata): Unit }) {
    println("32. " + r.z(new Titi)) // while this is ok
  }

  def m2[T](r: Object { def e(x: Tata): T; val x: Tata }) {
    println("33. " + r.e(r.x)) // and this too
  }

  class Rec3[T] {
    def e(x: T): T = x
  }

  def m3[T](r: Rec3[T], x: T) {
    println("33. " + r.e(x)) // and this too
  }

  Rec.g(11)

  this.l(Rec)
  this.mb(new Object{def e[T](x: T): T = x})
  this.m1(Rec)
  this.m2[Tata](Rec)
  this.m3[Tata](new Rec3[Tata], t1)
}

object test2 {
  class C extends { def f() { println("1") } }
  val x1 = new C
  x1.f()

  abstract class D extends { def f() }
  val x2 = new D { def f() { println("2") } }
  x2.f()

  val x3 = new { def f() { println("3") } }
  def run(x: { def f() }) { x.f() }
  run(x3)

  type T = { def f() }
  val x4 = new AnyRef { def f() { println("4") } } // ok!
  //val x4 = new T { def f() { println("4") } }        // error! (bug #1241)
  x4.f()

  val x5: T = new { def f() { println("5") } }
  x5.f()
}

object test3 {

  case class Exc() extends Exception

  object Rec {
    def f = throw Exc()
  }

  def m(r: { def f: Nothing }) =
    try {
      r.f
    }
    catch {
      case e: Exc => println("caught")
      case e: Throwable => println(e)
    }

  m(Rec)

}

object test4 {

  class A

  val aar = Array(new A, new A, new A)
  val nar = Array(1, 2)

  def f(p: {def size: Int}) = println(p.size)
  //def g[T <: {def size: Int}](p: T) = println(p.size) // open issue
  //def h[T <% {def size: Int}](p: T) = println(p.size) // open issue

  f(aar)
  f(nar)

  //g(aar)
  //g(nar)

  //h(aar)
  //h(nar)

}

object Test extends App {
  test1
  test2
  test3
  test4
}
