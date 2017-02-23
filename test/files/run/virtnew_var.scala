object Test extends App {
  trait Rep[x] {
    def selectDynamic[T](n: String): Rep[T] = error("")
  }

  // representation of a statically-known constant
  case class Const[T](x: T) extends Rep[T]

  // automatically lift strings into their representations
  implicit def liftString(x: String): Rep[String] = Const(x)
  implicit def liftInt(x: Int): Rep[Int] = Const(x)

  case class Variable[T]

  // to represent the self/this reference in a reified object creation
  case class Self[T] extends Rep[T]

  // this method is called by the virtualizing compiler
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = {
    val me = new Self[T]
    new Obj(me, args map {case (n, m, rhs) => (n, rhs(me))} toMap)
  }

  class Obj[T](self: Rep[T], fields: Map[String, Rep[_]]) extends Rep[T] {
    override def selectDynamic[T](n: String): Rep[T] = {
      val res = fields(n)
      println(self +" DOT "+ n + " = "+ res)
      res.asInstanceOf[Rep[T]]
    }
  }

  def __newVar[T](x: T): Variable[T] = error("meh")

  val foo: Rep[Struct { var xx: Int; val y: String }] = new Struct { var xx = 23; val y = "y" }
  println(foo.xx)
}
