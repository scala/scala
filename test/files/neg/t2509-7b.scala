// scalac: -Xsource:3.0
class Both[-A, +B]

trait Factory[A] {
  implicit def make: Both[A, A] = new Both[A, A]
}

trait X
object X extends Factory[X] {
  override implicit def make: Both[X, X] = super.make
}

class Y extends X
object Y extends Factory[Y] {
  // See test/files/pos/t2509-7a.scala ... discussion below
  // override implicit def make: Both[Y, Y] = super.make
}

object Test {
  def get(implicit ev: Both[Y, X]) = ev

  // There are two possible implicits here: X.make and Y.make, neither are
  // subtype of each other, so who wins?
  // - Under the old scheme it's X.make because `isAsGood` sees that X.make is defined
  // in X whereas Y.make is defined in Factory
  // - Under the new scheme it's ambiguous because we replace contravariance by covariance
  // in top-level type parameters so Y.make is treated as a subtype of X.make
  // In both schemes we can get Y.make to win by uncommenting the override for make in Y
  // (Y wins against X because `isDerived` also considers the subtyping relationships
  // of companion classes)
  get
}
