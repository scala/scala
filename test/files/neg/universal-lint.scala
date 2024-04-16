//> using options -Xlint -Werror

trait Test {
  def f = List("").map(_.isInstanceOf)
  def g = List("").map(_.asInstanceOf)
  def ok = List("").map(_.isInstanceOf[String])
  def or = List("").map(_.asInstanceOf[String])
}

import java.util.Spliterator
import java.util.stream._
import scala.collection.Stepper
sealed trait StreamShape[T, S <: BaseStream[_, _], St <: Stepper[_]] {
  final def fromStepper(st: St, par: Boolean): S = mkStream(st, par)
  protected def mkStream(st: St, par: Boolean): S
}
trait StreamShapeLowPriority1 {
  // reference
  implicit def anyStreamShape[T]: StreamShape[T, Stream[T], Stepper[T]] = anyStreamShapePrototype.asInstanceOf[StreamShape[T, Stream[T], Stepper[T]]]

  private[this] val anyStreamShapePrototype: StreamShape[AnyRef, Stream[AnyRef], Stepper[AnyRef]] = new StreamShape[AnyRef, Stream[AnyRef], Stepper[AnyRef]] {
    def mkStream(s: Stepper[AnyRef], par: Boolean): Stream[AnyRef] = StreamSupport.stream(s.spliterator.asInstanceOf[Spliterator[AnyRef]], par)
  }
}
