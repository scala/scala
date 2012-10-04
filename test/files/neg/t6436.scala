object quasiquotes {
  implicit def foo1(ctx: StringContext) = new { def q = ??? }
  implicit def foo2(ctx: StringContext) = new { def q = ??? }
}

object Test extends App {
  import quasiquotes._
  println(q"a")
}
