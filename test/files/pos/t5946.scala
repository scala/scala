//> using options -Yrangepos
//
object TestDep {
  class Ops(val g: scala.reflect.api.JavaUniverse) {
    def op[T: g.TypeTag] = ()
  }
  implicit def Ops(g: scala.reflect.api.JavaUniverse): Ops = new Ops(g)
  scala.reflect.runtime.universe.op[Int]
}
