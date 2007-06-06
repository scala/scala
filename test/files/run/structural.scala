object Test extends Application {

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

  def l (r: rt) = {

    Console.println(" 1. " + r.c)
    Console.println(" 2. " + r.a + 1)
    Console.println(" 3. " + r.d(o1))
    Console.println(" 4. " + r.e(t1))
    Console.println(" 5. " + (r.f(4) + 1))
    Console.println(" 6. " + r.f(4) + 1)
    Console.println(" 7. " + r.f(r.a))
    Console.println(" 8. " + r.v)
    r.v = r.v + 1
    Console.println("10. " + r.v)
    Console.println("11. " + r.h(()))
    Console.println("12. " + r.i(Array(1, 2, 3)))
    Console.println("13. " + r.j(Array(o1, o2)))
    Console.println("14. " + r.k(Array('1', '2')))
    Console.println("15. " + r.l(Array((), ())))
    Console.println("16. " + r.m(Array("one", "two")))
    Console.println("17. " + r.n(Array(t1, t2)))
    Console.println("18. " + (r.o(0) + 1))
    Console.println("19. " + (r.p(0).hashCode() > 0))
    Console.println("20. " + r.q(0))
    Console.println("21. " + r.r(0))
    Console.println("22. " + r.m(r.s))
    Console.println("23. " + r.t(0).tatMe)
    Console.println("24. " + r.u[Int](_+1,0))
    Console.println("25. " + r.y)
    Console.println("26. " + r.e(null))

  }

  /*def ma[T](r: Object{def e(x: T): T; val x: T}) {
    Console.println("30. " + r.e(r.x)) // static error
  }*/

  def mb(r: Object{def e[T](x: T): T}) {
    Console.println("31. " + r.e[Int](4)) // while this is ok
  }

  def m1(r: Object{def z(x: Tata): Unit}) {
    Console.println("32. " + r.z(new Titi)) // while this is ok
  }

  def m2[T](r: Object{def e(x: Tata): T; val x: Tata}) {
    Console.println("33. " + r.e(r.x)) // and this too
  }

  class Rec3[T] {
    def e(x: T): T = x
  }

  def m3[T](r: Rec3[T], x: T) {
    Console.println("33. " + r.e(x)) // and this too
  }

  Rec.g(11)

  this.l(Rec)
  this.mb(new Object{def e[T](x: T): T = x})
  this.m1(Rec)
  this.m2[Tata](Rec)
  this.m3[Tata](new Rec3[Tata], t1)
}