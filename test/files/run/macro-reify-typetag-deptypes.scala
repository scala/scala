object Test extends App {
  import scala.reflect.runtime.universe.{typeTag, typeOf}
  
  val a: AnyRef{ type T = Int } = new { type T = Int }
  println(typeTag[a.T])
  println(typeOf[a.T] <:< typeOf[Int]) //TODO this should be =:= every time (issue 10254)
  
  val b: AnyRef{ type F[x] = Option[x] } = new { type F[x] = Option[x] }
  println(typeTag[b.F[Int]])
  println(typeOf[b.F[Int]] <:< typeOf[Option[Int]])
  
  trait Foo { type T }
  val c: Foo { type T = String } = new Foo { type T = String }
  println(typeTag[c.T])
  println(typeOf[c.T] <:< typeOf[String])
  
  val d: Foo { type T = a.T } = new Foo { type T = a.T }
  println(typeTag[d.T])
  println(typeOf[d.T] <:< typeOf[Int])
}
