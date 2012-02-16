trait Bippy[@specialized(
  scala.Char, scala.Boolean, scala.Byte,
  scala.Short, scala.Int, scala.Long,
  scala.Float, scala.Double, scala.Unit,
  scala.AnyRef) T] { }

trait Bippy2[@specialized(Char, Boolean, Byte, Short, Int, Long, Float, Double, Unit, AnyRef) T] { }
