
/** Test that constructor operations are reordered correctly.  */
class Outer {

  object global {
    val x = 10;
  }

  class X extends {
    /* The constructor of X should set this.$outer to the outer instance
     * *before* calling the super constructors. This is tested by 
     * mixin M1, which tries to access global from the enclosing class.
     */
    val outer = Outer.this
  } with AnyRef with M1

  trait M1 { self: X =>
    Console.println(global.x);
    Console.println(outer.global.x);
  }

}

object Test extends AnyRef with App {
  val o = new Outer;

  new o.X;
}
