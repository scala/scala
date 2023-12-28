
//> using options -Werror -Wunused:imports

class X
class Y extends X
object A { implicit val x: X = new X }
object B { implicit val y: Y = new Y }
class C {
  import B._
  import A._ // missing unused warning (order of imports is significant)
  def t = implicitly[X]
}
