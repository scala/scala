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
//It compiles without errors, but T in B and T in C are completely unrelated types. 
