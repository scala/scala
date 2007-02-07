package scala.ref;

trait Reference[+T <: AnyRef] extends Function0[T] {
  def isValid : Boolean;
  def apply() : T;
  def get = if (!isValid) None else Some(apply());
  override def toString = if (!isValid) "<deleted>" else apply().toString;
  def clear : Unit;
  def enqueue : Boolean;
  def isEnqueued : Boolean;
}
