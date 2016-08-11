class A

class B extends A {
  def y: Int = 0
}

class B1 extends B
class B2 extends B

class C {
  def f: A = null
}

class D extends C {
  def s = ""
  override final val f = s match {
    case "" => new B1
    case _ => new B2
  }

  def m = f.y // doesn't compile anymore
}