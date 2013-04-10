object Test extends App {
  class C {
    class D {
      private def x = "D"
      def show = x
      class E {
    	println(x)
      }
    }
    
    val foo: D = {
      class D extends C.this.D {
        private def x = "foo.D"
    	class E {
    	  println(x)
    	}
      }
      new D
    }
  }
  val c = new C
  val d = c.foo
  println(d.show)
}
