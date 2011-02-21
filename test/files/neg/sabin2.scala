object Test extends App
 {
   abstract class Base {
     type T
     var x: T = _
     class Inner {
       def set(y: T) = x = y
       def get() = x
       def print() = println("Hello world")
     }
   }

   object IntBase extends Base { type T = Int }
   object StringBase extends Base { type T = String }

   val a : Base#Inner = new IntBase.Inner
   val b : Base#Inner = new StringBase.Inner

   a.print() // OK
   b.print() // OK

   a.set(b.get()) // Error
 }
