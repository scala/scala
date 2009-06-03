object Test extends Application {
  // TESTS

  // re-ordering
  test1(b = 2, a = "#")

  // mixing named and positional
  test1(a = 1, "*")
  test1(b = "(*", 23)

  // assignment / names
  var x = 0
  var y = 0
  test2(x = 1)
  test2(y = 1)
  test1(c = 0, b = "joke")


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


  // DEFINITIONS
  def test1(a: Int, b: String) = a +": "+ b
  def test2(x: Unit) = println("test2")
  def test3(a: Int, b: Int) = a + b
}

// only one overloaded alternative is allowed to have defaults
class A {
  def foo(a: Int = 0) = a
  def foo(b: String = "another") = b
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

// using names / defaults in super constructor call not allowed
class A1(x: Int, y: Int = 2)
class B1 extends A1(10)
class B2 extends A1(y = 20, x = 2)

// using names / defaults in self constructor call not allowed
class A2(x: String, y: Int = 10) {
  def this(a: Object) {
    this(y = 10, x = a.toString())
    println(x)
  }
}
class A3(x: String, y: Int = 11) {
  def this(b: Double, sep: String) {
    this(sep + b + sep)
  }
}

case class Fact(a: Int, b: String)(c: Int*)

class A5 { def foo(a: Object = "dlkf") = 0 }
class B5 extends A5 { override def foo(a: Object = new Object) = 1 }
