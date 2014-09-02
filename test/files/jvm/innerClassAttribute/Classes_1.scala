class A1 {
  class B
}

class A2 {
  object B
}

object A3 {
  class B1
  object B2
}

class A4 {
  def f(l: List[Int]): List[Int] = {
    l map (_ + 1)
  }
}

class A5 {
  def f(): Object = {
    object B
    B
  }
}

trait A6 {
  def hui = -6
  trait TT
}

class A7 extends A6

abstract class A8 extends A6 {
  def fish: TT
}

class A9 {
  class brick extends annotation.StaticAnnotation
}

class A10 {
  val a9 = new A9()
  // there's no reference to brick in the bytecode (only in the pickle), so there's no InnerClass attribute for it.
  @a9.brick def f = -7
}

class A11 {
  @JavaAnnot_1.Ann def f = -8
}

object A12 {
  object B {
    class C
  }
}

class A13 {
  def oak: A12.B.C = new A12.B.C
}

class A14 {
  def f = {
    val x: Object = {
      class K
      new K
    }
    x
  }
  def g = {
    val x: Object = new A6 { }
  }
}

object A15 {
  def f = {
    class B { // static (does not have an outer pointer)
      class C // non-static
    }
  }
}

class A16 {
  val x: A6 = {
    class U extends A6
    new A6 { }
  }

  {
    class V extends A6
    new A6 { }
  }
}

class A17 {
  object B {
    class C // not static, has an outer pointer.
  }
}
