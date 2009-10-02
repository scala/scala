trait Ring[T <: Ring[T]] {
  def +(that: T): T
  def *(that: T): T
}

class A extends Ring[A] {
  def +(that: A) = new A
  def *(that: A) = new A
}

class Poly[C <: Ring[C]](val c: C) extends Ring[Poly[C]] {
  def +(that: Poly[C]) = new Poly(this.c+that.c)
  def *(that: Poly[C]) = new Poly(this.c*that.c)
}

implicit def coef2poly[C <: Ring[C]](c: C) = new Poly(c)

val a = new A
val x = new Poly(new A)

x+a // works
a+x // works

val y = new Poly(new Poly(new A))

x+y*x // works
x*y+x // works
y*x+x // works

x+x*y
