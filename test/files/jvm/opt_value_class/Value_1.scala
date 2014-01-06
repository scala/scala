final class Opt[+A >: Null](val value: A) extends AnyVal {
  def get: A  = value
  def isEmpty = value == null
}
object Opt {
  final val None = new Opt[Null](null)
  def unapply[A >: Null](x: A): Opt[A] = if (x == null) None else Opt(x)
  def empty[A >: Null] = None
  def apply[A >: Null](value: A): Opt[A] = if (value == null) None else new Opt[A](value)
}

class ValueExtract {
  def unapply(x: Any): Opt[String] = x match {
    case _: String  => Opt("String")
    case _: List[_] => Opt("List")
    case _: Int     => Opt("Int")
    case _          => Opt.None
  }
}

class Direct {
  def unapply(x: Any): String = x match {
    case _: String  => "String"
    case _: List[_] => "List"
    case _: Int     => "Int"
    case _          => null
  }
}
