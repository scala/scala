object Test extends App {
  abstract class Record extends Struct

  trait OptiML {
    case class Rep[+T](const: T) {
      def selectDynamic[T](n: String): Rep[T] = error(n)
    }
    implicit def unit[T](const: T): Rep[T] = Rep(const)
    def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
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
    case class Rep[+T](const: T) {
      def selectDynamic[T](n: String): Rep[T] = error(n)
    }
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

  val x: Record { val foo: String } = OptiML {
    val f = (x: Int) => "foo "+ x
    new Record { val foo = f(10) }
  }
  val y: String = OptiQL {
    unit(x).foo
  }
}
