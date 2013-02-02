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
  import path.Dep

  def apply(dep: Dep)(d2: dep.type, foo: Int): (Dep, d2.type) = (d2, d2)

  // This generates dodgy code; note `ValueClass.this`:
  //
  // final def bounds$extension[D >: Nothing <: ValueClass.this.path.Dep,
  //                            P >: Nothing <: Path]
  //                            ($this: ValueClass[P])
  //                            (dep: D)
  //                            (d2: dep.type, foo: Int): (D, d2.type) = scala.Tuple2.apply[D, d2.type](d2, d2);
  //
  // Nothing crashes down the line, but it certainly doesn't conform to best-practices.
  //
  // An better alternative would be to add a type parameter for the (singleton) type of
  // the wrapped value.
  def bounds[D <: Dep](dep: D)(d2: dep.type, foo: Int): (D, d2.type) = (d2, d2)
}

