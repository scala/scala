import scala.language.reflectiveCalls

case class ContextProperty(value: Any) {
  type HasToInt = { def toInt:Int }

  def toInt: Int = value match {
    case n: HasToInt => n.toInt
  }
}

// was:
// error:7: error during expansion of this match (this is a scalac bug).
// The underlying error was: type mismatch;
//  found   : Boolean(true)
//  required: AnyRef
//   def toInt: Int = value match {