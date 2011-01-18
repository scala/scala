package scala

/** A trait that supports dynamic invocations. Instances `x` of this trait
 *  allow calls `x.meth(args)` for arbitrary method names `meth` and argument lists
 *  `args`. If a call is not natively supported by `x`, it is rewritten to
 *  `x.invokeDynamic("meth", args)`.
 */
trait Dynamic {

  /** The dynamic invocation operation
   *  @param name  The name of the invoked method
   *  @param args  The arguments to the method
   */
  def invokeDynamic(name: String, args: Any*): Any

  /** Returns the underlying value typed as an instance of type T
   *  @param T  The target type
   */
  def typed[T]: T
}


