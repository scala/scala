abstract class Template[T <: AnyRef](private val t: T) {

//  type Repr[A<:AnyRef]<:Template[T]
  type Repr[T]<:Template[T]

  def access1(timeout: Int): Repr[T] = this.asInstanceOf[Repr[T]]
  def access2: Repr[T] = this.asInstanceOf[Repr[T]]
  val access3: Repr[T] = this.asInstanceOf[Repr[T]]
  def access4(v: Repr[T]): Repr[T] = this.asInstanceOf[Repr[T]]
  def access5(x: X): Repr[T] = this.asInstanceOf[Repr[T]]
  def access5(x: Y): Repr[T] = this.asInstanceOf[Repr[T]]

  def withReadModifiers(readModifiers:Int): Repr[T] = this.asInstanceOf[Repr[T]]
}

class Curve

class CurveTemplate [T <: Curve](t: T) extends Template(t) {
//  type Repr[A<: AnyRef] = CurveTemplate[T]
  type Repr = CurveTemplate[T]
}

class Base
class X extends Base
class Y extends Base


object Example {
 def test1() {
   new CurveTemplate(new Curve).access1(10)

   new CurveTemplate(new Curve).access2

   new CurveTemplate(new Curve).access3

   new CurveTemplate(new Curve).access4(null)

   new CurveTemplate(new Curve).access5(new X)

   ()

 }

 def test2() {
   new CurveTemplate(new Curve).access1(10).withReadModifiers(1)

   new CurveTemplate(new Curve).access2.withReadModifiers(1)

   new CurveTemplate(new Curve).access3.withReadModifiers(1)

   new CurveTemplate(new Curve).access4(null).withReadModifiers(1)

   new CurveTemplate(new Curve).access5(new X).withReadModifiers(1)
 }
}
