abstract class A {
 def f(x : this.type) : B
}

class B extends A {
 override def f(x : this.type) : B = x
}

class C extends A {
 override def f(x : this.type) : B = null
}

object Program {
  def main(args : Array[String]) {
    (new B: A).f(new C)
  }
}
