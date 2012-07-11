abstract class Template[T <: AnyRef](private val t: T) {

  type Repr[T]<:Template[T]

  def withTimeout(timeout:Long): Repr[T] = this.asInstanceOf[Repr[T]]
  def withReadModifiers(readModifiers:Int): Repr[T] = this.asInstanceOf[Repr[T]]
}

class Curve

class CurveTemplate [T <: Curve](t: T) extends Template(t) {
  type Repr = CurveTemplate[T]
}


object Example {
 new CurveTemplate(new Curve).withTimeout(2000L).withReadModifiers(0)
}
