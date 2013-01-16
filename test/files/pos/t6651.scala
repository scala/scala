class YouAreYourself[A <: AnyRef](val you: A) extends AnyVal {
  def yourself: you.type = you
}

object Test {
	val s = ""
	val s1: s.type = new YouAreYourself[s.type](s).yourself
}

trait Path {
  type Dep <: AnyRef
}

final class ValueClass[P <: Path](val path: P) extends AnyVal {
  import path._
  def apply(dep: Dep)(d2: dep.type, foo: Int): (Dep, d2.type) = (d2 ,d2)
}

object TestValueClass {
	object P extends Path {
		type Dep = String
	}

	val s: String = ""
	new ValueClass(P).apply(s)(s, 0): (String, s.type)
}
