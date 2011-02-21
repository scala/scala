object Test extends App {
 trait A
 trait B extends A

 class C {
   type D
   trait E { type T >: B <: A; val x : T }
   // This is currently correctly disallowed
   // val y : (D with E)#T = y
   val y : D with E = y
   var sneak = { () => y.x }
   sneak = { () => new B { } }
 }

 class F extends C {
   trait G
   trait H { type T = G }
   type D = H
   def frob(arg : G) : G = arg
   frob(sneak())
 }

 new F
}
