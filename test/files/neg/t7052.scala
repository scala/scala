
class A {
  def apply(xs: Int*) = 42
}

/* name clash between defined and inherited member:
 */
class B extends A {
  def apply(xs: Seq[Int]) = 27
}

/* method apply overrides nothing.
class C extends A {
  override def apply(xs: Seq[Int]) = 17
}
 */

// ok because different return type
class D extends A {
  def apply(xs: Seq[Int]) = "42"
}
