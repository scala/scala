import scala.reflect.runtime.universe._

class D[V <: Variable]

class ID[V<:IV] extends D[V] {
  type E = V#ValueType
  def index(value:E) : Int = 0
  // Comment this out to eliminate crash.  Or see below
  def index(values:E*) : Iterable[Int] = null
}

abstract class Variable {
  type VT <: Variable
  def d : D[VT] = null
}

abstract class PV[T](initval:T) extends Variable  {
  type VT <: PV[T]
  type ValueType = T
}

trait IV extends Variable  {
  type ValueType
}

abstract class EV[T](initval:T) extends PV[T](initval) with IV {
  type VT <: EV[T]
  override def d : ID[VT] = null
  // Comment this out to eliminate crash
  protected var indx = d.index(initval)
}