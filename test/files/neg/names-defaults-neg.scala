object Test extends Application {
  // TESTS

  // re-ordering
  test1(b = 2, a = "#")

  // mixing named and positional
  test1(b = "(*", 23)

  // assignment / names
  var x = 0
  var y = 0
  test2(x = 1)
  test2(y = 1)
  test1(c = 0, b = "joke")
  test7((m = 1))  // named arguments must be top-level assignments
  test7({m = 1})
  test7 { m = 1 } // no named arguments in argument block
  test8(x = 1)

  // argument specified twice
  test1(1, a = 2)
  test1(b = 1, b = "2")

  // error message when there are too many argument lists (not very nice..)
  test3(b = 3, a = 1)(3)



  // overloading resolution
  object t1 {
    def f(a: Int, b: String) = "first"
    def f(b: String, a: Int) = "second"
  }
  t1.f(b = "dkljf", a = 1)


  object t3 {
    def f(a1: Int) = "first"
    def f(a2: Int)(b: Int) = "second"
  }
  t3.f(1)
  t3.f(1)(2)

  object t7 {
    def g(a: C, b: Int*) = "third"
    def g(a: B) = "fourth"
  }
  t7.g(new C()) // ambigous reference

  // vararg
  def test5(a: Int, b: String*) = a
  test5(a = 1, b = "dkjl", b = "dkj")
  test5(1, "2", b = 3)
  test5(b = "dlkj")

  object t8 {
    def f(a: Int, b: Object) = "first"
    def f(b: String, a: Int) = "second"
  }
  println(t8.f(a = 0, b = "1")) // ambigous reference


  // case class copy does not exist if there's a vararg
  val fac = Fact(1)(2, 3)
  val facc = fac.copy(b = "dlkfj")()

  // no defaults in patterns
  A1() match { case A1(_) => () }


  // return types of default getters

  // definition compiles, but default cannot  be used, it doesn't conform
  def test4[T[P]](x: T[T[List[T[X forSome { type X }]]]] = List(1,2)) = x
  test4()

  // doesn't compile
  def test6[T](x: List[List[T]] = List(1,2)) = x

  // correct error message
  new A2[String]()

  object t3648 {
    class C(val s: String = "")
    object C extends C()
  }

  // deprecated names
  def deprNam1(x: Int, @deprecatedName('x) y: String) = 0
  def deprNam2(a: String)(@deprecatedName('a) b: Int) = 1
  def deprNam3(@deprecatedName('x) a: Int, @deprecatedName('y) b: Int) = a + b
  deprNam3(y = 10, b = 2)


  // DEFINITIONS
  def test1(a: Int, b: String) = a +": "+ b
  def test2(x: Unit) = println("test2")
  def test3(a: Int, b: Int) = a + b
  def test7(m: Int) = m
  def test8[T](x: => T) = println("test8")
}

class B {
  def foo(a: Int) = a
  def bar(u: String = "ldksj") = u
}

class C extends B {
  override def foo(a: Int = 1092) = a
  def foo(b: String = "lskdfj")

  def bar(i: Int = 129083) = i
}

case class Fact(a: Int, b: String)(c: Int*)

case class A1(x: Int = 1, y: String = "2")

class A2[T](a: T = 1)


// anonymous functions
object anfun {
  var var2 = 0
  def delay(var2: => Unit) { var2 }
  delay(var2 = 40)

  def testAnnFun(a: Int, b: String) = println(a +": "+ b)
  val taf2: Int => Unit = testAnnFun(a = _, b = get("+"))
  val taf3 = testAnnFun(b = _: String, a = get(8))
  val taf4: (Int, String) => Unit = testAnnFun(_, b = _)
}

object t3685 {
  object t { def f(x: Int) = x }

  def t1 { def x = t.f(x = 1) }
  def t2 { val x = t.f(x = 1) }
  def t3 { var x = t.f(x = 1) }
  object t4 { def x = t.f(x = 1) }
  object t5 { val x = t.f(x = 1) }
  object t6 { var x = t.f(x = 1) }
  class t7 { def x = t.f(x = 1) }
  class t8 { val x = t.f(x = 1) }
  class t9 { var x = t.f(x = 1) }

  def t10 { def x: Int = t.f(x = 1) }
  def t11 { val x: Int = t.f(x = 1) }
  def t12 { var x: Int = t.f(x = 1) }
  class t13 { def x: Int = t.f(x = 1) }
  class t14 { val x: Int = t.f(x = 1) }
  class t15 { var x: Int = t.f(x = 1) }


  object u { def f[T](x: T) = 100 }

  def u1 { def x = u.f(x = 1) }
  def u2 { val x = u.f(x = 1) }
  def u3 { var x = u.f(x = 1) }
  def u4 { def x = u.f(x = "23") }
  def u5 { val x = u.f(x = "32") }
  def u6 { var x = u.f(x = "32") }
  def u7 { def x: Int = u.f(x = 1) }
  def u8 { val x: Int = u.f(x = 1) }
  def u9 { var x: Int = u.f(x = 1) }
  def u10 { def x: Int = u.f(x = "32") }
  def u11 { val x: Int = u.f(x = "32") }
  def u12 { var x: Int = u.f(x = "32") }

  class u13 { def x = u.f(x = 1) }
  class u14 { val x = u.f(x = 1) }
  class u15 { var x = u.f(x = 1) }
  class u16 { def x: Int = u.f(x = 1) }
  class u17 { val x: Int = u.f(x = 1) }
  class u18 { var x: Int = u.f(x = 1) }
  class u19 { def x: Int = u.f(x = "32") }
  class u20 { val x: Int = u.f(x = "32") }
  class u21 { var x: Int = u.f(x = "32") }
}
