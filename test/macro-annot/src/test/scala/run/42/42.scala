package issue42

object M {
  implicit class TimestampOps[@specialized(Int, Long) A](val i: A){}
}