package scala

abstract class MatchingStrategy[M[+x]] {
  def zero: M[Nothing]
  def one[T](x: T): M[T]
  def guard[T](cond: Boolean, then: => T): M[T] // = if(cond) one(then) else zero
  def altFlatMap[T, U](f: T => M[U])(a: M[U], b: M[T]): M[U] // = a orElse b.flatMap(f) -- can't easily&efficiently express M[T] should have flatMap and orElse
  def runOrElse[T, U](x: T)(f: T => M[U]): U
  def isSuccess[T, U](x: T)(f: T => M[U]): Boolean

  // find the first alternative to successfully flatMap f
  // to avoid code explosion due to alternatives
  def or[T, U](f: T => M[U], alts: M[T]*) = (alts foldLeft (zero: M[U]))(altFlatMap(f))

  def caseResult[T](x: T): M[T] = one(x) // used as a marker to distinguish the RHS of a case (case pat => RHS) and intermediate successes
  // when deriving a partial function from a pattern match, we need to
  // distinguish the RHS of a case, which should not be evaluated when computing isDefinedAt,
  // from an intermediate result (which must be computed)

}

object MatchingStrategy {
  implicit object OptionMatchingStrategy extends MatchingStrategy[Option] {
    type M[+x] = Option[x]
    @inline def guard[T](cond: Boolean, then: => T): M[T] = if(cond) Some(then) else None
    @inline def zero: M[Nothing] = None
    @inline def one[T](x: T): M[T] = Some(x)
    @inline def altFlatMap[T, U](f: T => M[U])(a: M[U], b: M[T]): M[U] = a orElse b.flatMap(f)
    @inline def runOrElse[T, U](x: T)(f: T => M[U]): U = f(x) getOrElse (throw new MatchError(x))
    @inline def isSuccess[T, U](x: T)(f: T => M[U]): Boolean = !f(x).isEmpty
  }
}