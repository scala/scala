object TestDep {
  class Ops(val g: scala.reflect.api.JavaUniverse) {
    def op[T: g.TypeTag] = ()
  }
  def Ops(g: scala.reflect.api.JavaUniverse): Ops = new Ops(g)
  Ops(scala.reflect.runtime.universe).op[Int]
}
