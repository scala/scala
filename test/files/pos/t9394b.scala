sealed trait Base
case class Up() extends Base
case class Down() extends Base
 
sealed trait Tracker[B <: Base] {
 
  def bar = (this: Tracker[_ <: B]) match {
    case UpTracker() => ???
    case DownTracker() => ???
  }
 
}
case class UpTracker() extends Tracker[Up]
case class DownTracker() extends Tracker[Down]
