package scala

/** A marker trait that enables dynamic invocations. Instances `x` of this trait
 *  allow calls `x.meth(args)` for arbitrary method names `meth` and argument lists
 *  `args`.  If a call is not natively supported by `x`, it is rewritten to
 *  `x.applyDynamic("meth", args)`.
 *
 *  As of scala 2.9, scalac must receive the -Xexperimental optional for Dynamic
 *  to receive this treatment.
 */
trait Dynamic


