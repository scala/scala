//> using options -Wconf:cat=w-flag-tostring-interpolated:e -Wtostring-interpolated

case class C(x: Int)

trait T {
  def c = C(42)
  def f = f"$c" // warn
  def s = s"$c" // warn
  def r = raw"$c" // warn

  def format = f"${c.x}%d in $c or $c%s" // warn using c.toString // warn

  def bool = f"$c%b" // warn just a null check

  def oops = s"${null} slipped thru my fingers" // warn

  def ok = s"${c.toString}"

  def sb = new StringBuilder().append("hello")
  def greeting = s"$sb, world" // warn
}

class Mitigations {

  val s = "hello, world"
  val i = 42
  def shown() = println("shown")

  def ok = s"$s is ok"
  def jersey = s"number $i"
  def unitized = s"unfortunately $shown" // warn accidental unit value
  def funitized = f"unfortunately $shown" // warn accidental unit value

  def nopct = f"$s is ok"
  def nofmt = f"number $i"
}
