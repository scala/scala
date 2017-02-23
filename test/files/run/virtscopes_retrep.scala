object Test extends App {
  trait OptiML {
    case class Rep[+T](const: T)
    implicit def unit[T](const: T): Rep[T] = Rep(const)
  }
  trait OptiMLExp[R] extends OptiML {
    def apply: Rep[R]
    def result: R = {
      val r = apply
      println("OptiML " + r.const)
      r.const
    }
  }

  trait OptiQL {
    case class Rep[+T](const: T)
    implicit def unit[T](const: T): Rep[T] = Rep(const)
  }
  trait OptiQLExp[R] extends OptiQL {
    def apply: Rep[R]
    def result: R = {
      val r = apply
      println("OptiQL " + r.const)
      r.const
    }
  }

  def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp[R], R](b)
  def OptiQL[R](b: => R) = new Scope[OptiQL, OptiQLExp[R], R](b)

  val x: String = OptiML {
    val f = (x: Int) => "foo "+ x
    unit(f(10))
  }
  val y: String = OptiQL {
    unit(x)
  }
}
