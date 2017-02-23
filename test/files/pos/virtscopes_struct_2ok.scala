object ScopeTest {
  abstract class Record extends Struct

  trait Base/* extends EmbeddedControls*/ {
    type Rep[+T]
    protected def unit[T:Manifest](x: T): Rep[T]
  }

  trait RecordOps extends Base {
    def __new[T:Manifest](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] =
      error("")
    class RecordOps(record: Rep[Record]) {
      def selectDynamic[T:Manifest](n: String): Rep[T] = error(n)
    }
    implicit def recordToRecordOps(record: Rep[Record]) = new RecordOps(record)
  }

  trait RecordOpsExp extends RecordOps {
    type Rep[+T] = Exp[T]

    case class Exp[+T](const: T)
    implicit def unit[T:Manifest](const: T): Rep[T] = Exp(const)
  }

  trait OptiML extends RecordOps
  trait OptiMLExp[R] extends OptiML with RecordOpsExp {
    def apply: R
    def result: R = {
      val r = apply
      r
    }
  }

  def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp[R], R](b)

  type Foo = Record { val foo: String; val bar: Int }

  val x: Int = OptiML {
    val f = (x: Int) => "foo "+ x
    val z: Rep[Foo] = new Foo { val foo = unit(f(10)); val bar = unit(1) }
    println(z.foo)
    println(z.bar)
    10
  }
}
