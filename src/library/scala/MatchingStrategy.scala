package scala

abstract class MatchingStrategy[M[+x]] {
  // runs the matcher on the given input
  def runOrElse[T, U](in: T)(matcher: T => M[U]): U

  def zero: M[Nothing]
  def one[T](x: T): M[T]
  def guard[T](cond: Boolean, then: => T): M[T]
  def isSuccess[T, U](x: T)(f: T => M[U]): Boolean // used for isDefinedAt

  def caseResult[T](x: T): M[T] = one(x) // used as a marker to distinguish the RHS of a case (case pat => RHS) and intermediate successes
  // when deriving a partial function from a pattern match, 
  // we need to distinguish the RHS of a case, which should not be evaluated when computing isDefinedAt,
  // from an intermediate result (which must be computed)
}

object MatchingStrategy {
  implicit object OptionMatchingStrategy extends MatchingStrategy[Option] {
    type M[+x] = Option[x]
    @inline def runOrElse[T, U](x: T)(f: T => M[U]): U = f(x) getOrElse (throw new MatchError(x))
    @inline def zero: M[Nothing] = None
    @inline def one[T](x: T): M[T] = Some(x)
    @inline def guard[T](cond: Boolean, then: => T): M[T] = if(cond) Some(then) else None
    @inline def isSuccess[T, U](x: T)(f: T => M[U]): Boolean = !f(x).isEmpty
  }
}