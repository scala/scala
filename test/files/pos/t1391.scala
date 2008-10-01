package sandbox

 class hierarOverload {

 /*
  * Template structure - using abstract types.
 */
   trait AB {
     type TA <: A
     type TB <: B

     protected trait A {
       val entities : List[TB]
     }

     protected trait B {
       var group : TA
     }
   }

 /*
  * Template instantiation in an object to ease use and globally define
 abstract types
 */
   object NAnB extends AB {
     type TB = nB
     type TA = nA

     class nA extends A {
       val entities = List[nB]()
     }

     class nB extends B {
       var group = new nA
     }
   }

   def foo () {
     val t = new NAnB.nA
     println(t)
   }

 }
