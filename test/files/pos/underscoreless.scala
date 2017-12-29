
trait T {
  def i = 17
  def f = 42 match {
    case if i > 0     => "ok"
    case n if n == 42 => "yup"
    case 41           => "not quite"
  }
}
