
class Test {
  val x = 42
  def g = s"abc\$x"  // rt
  def h = f"abc\"
  def i = f"abc\\"   // ok
  def j = f"abc\\\"
  def k = f"abc\${x}\${x}"
  def m = s"""abc\"def""" // ok
  def n = s"""abc\$x"""   // rt
  def p = s"\"       // rt
  def q = s"""\"""   // rt
}
