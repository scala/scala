object Test extends App {
  import scala.reflect.runtime.universe.typeOf
  
  val a: AnyRef{ type T } = new { type T }
  typeOf[a.T]
  
  val b: AnyRef{ type F[x] } = new { type F[x] }
  typeOf[b.F[Int]]
  
  val c: AnyRef{ type T >: Some[Int] <: Option[Int] } = new { type T >: Some[Int] <: Option[Int] }
  typeOf[c.T]
  
  val d: AnyRef{ type A; type T = A } = new { type A; type T = A }
  typeOf[d.T]
  
  val e: AnyRef{ type T = a.T } = new { type T = a.T }
  typeOf[e.T]
  
  val f = locally { class Local; new { type T = Local } }
  typeOf[f.T]
}
