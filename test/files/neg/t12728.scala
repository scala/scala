//> using options -Werror -Xlint

class C {
  val u = ()
  val i = 42
  val l = 42L
  val c = 'c'
  val x = null: Any

  println(u.!=(x))
  println(u.##)
  println(u.==(x))
  println(u.asInstanceOf[Any])
  println(u.equals(x))
  println(u.hashCode)
  println(u.isInstanceOf[Any])
  println(u.toString)
  println(i.toString)

  println(i.isNaN)
  println(i.isInfinity)
  println(i.isInfinite)
  println(i.isFinite)
  println(i.isPosInfinity)
  println(i.isNegInfinity)
  println(i.round)
  println(i.ceil)
  println(i.floor)

  println(l.isNaN)
  println(l.isInfinity)
  println(l.isInfinite)
  println(l.isFinite)
  println(l.isPosInfinity)
  println(l.isNegInfinity)
  println(l.round)
  println(l.ceil)
  println(l.floor)

  println(c.isNaN)
  println(c.isInfinity)
  println(c.isInfinite)
  println(c.isFinite)
  println(c.isPosInfinity)
  println(c.isNegInfinity)
  println(c.round)
  println(c.ceil)
  println(c.floor)
}

class UseCase {
  def f = new scala.StringBuilder("hi").setCharAt(0, 'H').toString        // "Hi"

  def g = new java.lang.StringBuilder("hi").setCharAt(0, 'H').toString    // "()"
}
