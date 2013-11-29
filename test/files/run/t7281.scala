import scala.runtime.ValueType

object Test extends App {

  // Works automatically for AnyVals ...
  assert(classOf[RichException].isAnnotationPresent(classOf[ValueType]))

  // ... but can also be added manually
  @ValueType
  class Complex(real: Double, imag: Double)
  assert(classOf[Complex].isAnnotationPresent(classOf[ValueType]))
}
