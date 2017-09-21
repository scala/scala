class Cell(val ambiguousName: Option[String])

class Test {
  def wrap(f: Any): Nothing = ???
  wrap {
    val c = new Cell(ambiguousName = Some("bla"))
    val ambiguousName = c.ambiguousName
  }
}
