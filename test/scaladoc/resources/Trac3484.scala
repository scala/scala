class cbf[A, B, C]

/**
 *  @define Coll Traversable
 *  @define bfreturn $Coll
 */
class Collection[A] {
  /** What map does...
   *
   *  $bfreturn
   *  @usecase def map[B](f: A => B): $bfreturn[B]
   *
   */
  def map[B, That](f: A => B)(implicit fact: cbf[Collection[A], B, That]) =
    null
}

/**
 *  @define b John
 *  @define a Mister $b
 */
class SR704 {
  /**
   *  Hello $a.
   */
  def foo = 123
}
