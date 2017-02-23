object Test extends App {
  trait Rep[+x] {
    // def __newVar[T](x: T): Rep[T] = error("")
    def selectDynamic[T](n: String): Rep[T] = error("")
    def applyDynamic[T](n: String): Rep[T] = error("")
    def updateDynamic[T](n: String)(v: Rep[Any]): Rep[Unit] = error("")
  }

  // representation of a statically-known constant
  case class Const[T](x: T) extends Rep[T]
  implicit def liftInt(x: Int): Rep[Int] = Const(x)
  implicit def intToIntOps(x: Int) = new IntOps(Const(x))
  implicit def repIntToIntOps(x: Rep[Int]) = new IntOps(x)

  case class Times(lhs: Rep[Int], rhs: Rep[Int]) extends Rep[Int]
  class IntOps(lhs: Rep[Int]) {
    def *(rhs: Rep[Int]) = Times(lhs, rhs)
  }

  // to represent the self/this reference in a reified object creation
  case class Self[T]() extends Rep[T]

  // this method is called by the virtualizing compiler
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = {
    val me = new Self[T]()
    new Obj(me, args map {case (n, b, rhs) => (n, rhs(me))} toMap)
  }

  class Obj[T](self: Rep[T], fields: Map[String, Rep[_]]) extends Rep[T] {
    override def selectDynamic[T](n: String): Rep[T] = {
      val res = fields(n)
      println(self +" DOT "+ n + " = "+ res)
      res.asInstanceOf[Rep[T]]
    }
  }

  val foo = new Struct {
      var xx = 2
      var yy = {xx = xx * 2; xx} // TODO: xx *= 2
    }

  val bar = new Struct {
      val xx = 2
      var yy = Const(() => xx * 2)
    }

  // println(bar.xx)
}
