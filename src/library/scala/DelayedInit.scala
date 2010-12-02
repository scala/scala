package scala

/** Classes and traits inheriting the `DelayedInit` marker trait
 *  will have their initialization code rewritten as follows.
 *  <code>   becomes   delayedInit(<code>)
 *  Initialization code comprises all statements and all value definitions
 *  that are executed during initialization.
 */
trait DelayedInit {
  def delayedInit(x: => Unit): Unit
}

