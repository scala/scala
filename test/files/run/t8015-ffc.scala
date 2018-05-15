
object Test extends App {
  val ms = """This is a long multiline string
  with \u000d\u000a CRLF embedded."""
  
  assert(ms contains "\r\n CRLF", "no CRLF")
  assert(ms.lines.size == 3, s"lines.size ${ms.lines.size}")
  
}
