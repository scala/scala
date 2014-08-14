package d.e

import c.B

case class C private(b: B)

object C {
	def apply(int: Int) = new C(B(int))
}