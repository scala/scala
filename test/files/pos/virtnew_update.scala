object Test extends App {
  trait Rep[x] {
    def selectDynamic[T](n: String): Rep[T] = error("")
    def applyDynamic[T](n: String) = selectDynamic(n)
    def updateDynamic[T](n: String)(rhs: T) = error("")
  }

  // representation of a statically-known constant
  case class Const[T](x: T) extends Rep[T]

  // automatically lift strings into their representations
  implicit def liftString(x: String): Rep[String] = Const(x)
  implicit def liftInt(x: Int): Rep[Int] = Const(x)

  case class Var[T, U](self: Rep[T], x: U) extends Rep[U]

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
  val foo: Rep[Struct { var xx: Int; val y: String }] = new Struct { var xx = 23; val y = "y" }
  foo.xx = 3 // this works because the row has a mutable member xx of the right type -- otherwise it'll fail to typecheck (see the neg/ case)
  println(foo.xx = 3) // check that nested assigns work too
  println(foo.xx)

  (new Dynamic {
    def updateDynamic(n: String)(rhs: Any) = error("")
  }).notInAStruct = 123 // make sure the regular applyDynamic (on targets of type Dynamic works too)
}
