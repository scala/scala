
trait T {
  def s = { println("SSS") ; "hello" }
  def ok = String.format("%s, %<s", s)
  def oops = f"$s%s $<s"
}
