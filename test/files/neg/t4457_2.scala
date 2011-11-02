object ImplicitConvAmbiguity2 {

  class N[T]
  class NE[T] extends N[T]
  class NN[T] extends N[T]
  class NQ[T] extends N[T]
  class NZ[T] extends N[T]
  class AA[A]
  class BB[A]

  implicit def conv1(i: Float) = new NE[Float]
  implicit def conv3(op: AA[java.util.TooManyListenersException]) = new N[java.util.TooManyListenersException]
  implicit def conv4(op: AA[Float]) = new N[Float]
  implicit def conv7(i: Float) = new NZ[Float]
  implicit def conv5(e: BB[java.util.GregorianCalendar]) = new N[java.util.GregorianCalendar]

  def aFunc[A](a: NE[A]) = new AA[A]
  def aFunc[A](a: NZ[A]) = new AA[A]

  def aFunc[A](a: NN[A]) = new BB[A]

  def aFunc[A](a: NQ[A]) = new BB[A]

  def bFunc[T](e1: N[T]) = {}
  
  def typeMe2 {
    val x = aFunc(4F)
    bFunc(x)
  }
  def typeMe1 {
    bFunc(aFunc(4F))
  }
}
