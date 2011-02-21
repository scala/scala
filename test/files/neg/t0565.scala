object Test extends App {

  class MacGuffin

  object A {
     var x : { type T >: Null <: AnyRef;
               val y : T;
               def z (w : T) : T } =
             new { type T = String;
                   val y = "foo";
                   def z (w : String) = w + "bar" }
     lazy val u = { println("u evaluated"); x }
     def foo (v : => u.type#T) : u.type#T = {
       x = new { type T = MacGuffin;
                 val y = new MacGuffin;
                 def z (w : MacGuffin) = w }
       u.z(v)
     }
  }

  A.foo(A.u.y)
}
