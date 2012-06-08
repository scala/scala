class Test {
  object severity extends Enumeration
  class Severity(val id: Int) extends severity.Value
  val INFO    = new Severity(0)
  val WARNING = new Severity(1)

  (0: Int) match {
    case WARNING.id =>
    case INFO.id => // reachable
    case WARNING.id => // unreachable
  }
}
