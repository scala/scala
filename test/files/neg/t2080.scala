trait A {
 type T
 def f(x : T) : T
}

trait B extends A {
 trait T { }
 override def f(x : T) : T = x
}

object C extends B {
 override trait T {
   def g {  }
 }
 override def f(x : T) : T = { x.g; x }
}

