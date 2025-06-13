//> using options -Werror -Wtostring-interpolated
///> using options -Wconf:cat=w-flag-tostring-interpolated:e -Wtostring-interpolated

case class C(x: Int)

trait T {
  def c = C(42)
  def f = f"$c" // warn
  def s = s"$c" // warn
  def r = raw"$c" // warn

  def format = f"${c.x}%d in $c or $c%s" // warn using c.toString // warn

  def ok = s"${c.toString}"

  def sb = new StringBuilder().append("hello")
  def greeting = s"$sb, world" // warn

  def literally = s"Hello, ${"world"}" // nowarn literal, widened to String

  def bool = f"$c%b" // warn just a null check (quirk of Java format)

  def oops = s"${null} slipped thru my fingers" // warn although conforms to String

  def exceptionally = s"Hello, ${???}" // warn although conforms to String
}

class Mitigations {

  val s = "hello, world"
  val i = 42
  def shown = println("shown") // nowarn parenless Unit def because that is at refchecks

  def ok = s"$s is ok"
  def jersey = s"number $i"
  def unitized = s"unfortunately $shown" // warn accidental unit value
  def funitized = f"unfortunately $shown" // warn accidental unit value

  def nopct = f"$s is ok"
  def nofmt = f"number $i"
}

class Branches {

  class C {
    val shouldCaps = true
    val greeting = s"Hello ${if (shouldCaps) "WORLD" else "world"}"
  }

  class D {
    val shouldCaps = true
    object world { override def toString = "world" }
    val greeting = s"Hello ${if (shouldCaps) "WORLD" else world}" // warn
  }

  class E {
    def x = 42
    val greeting = s"Hello ${x match { case 42 => "WORLD" case 27 => "world" case _ => ??? }}"
  }

  class F {
    def x = 42
    object world { override def toString = "world" }
    val greeting = s"Hello ${x match { case 42 => "WORLD" case 27 => world case _ => ??? }}" // warn
  }

  class Z {
    val shouldCaps = true
    val greeting = s"Hello ${if (shouldCaps) ??? else null}" // nowarn quirk, no string result
  }

}
