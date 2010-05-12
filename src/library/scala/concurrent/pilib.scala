/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.concurrent

/** <p>
 *    Library for using Pi-calculus concurrent primitives in
 *    <a href="http://scala-lang.org/" target="_top">Scala</a>. As an
 *    example, the definition of a two-place buffer using the <code>pilib</code>
 *    library looks like:
 *  </p><pre>
 *  <b>def</b> Buffer[a](put: Chan[a], get: Chan[a]) {
 *    <b>def</b> B0 { choice ( put * { x => B1(x) } ) }
 *    <b>def</b> B1(x: a) { choice ( get(x) * B0, put * { y => B2(x, y) } ) }
 *    <b>def</b> B2(x: a, y: a) { choice ( get(x) * B1(y) ) }
 *    B0
 *  }
 *  </pre>
 *
 *  @see     <a href="http://scala-lang.org/docu/papers.html" target="_top">
 *           PiLib: A Hosted Language for Pi-Calculus Style Concurrency</a>
 *  @author  Vincent Cremet, Martin Odersky
 *  @version 1.0
 */
object pilib {

  import TaskRunners.threadRunner

  //////////////////////////////// SPAWN /////////////////////////////////

  /**
   * Run several processes in parallel using the following syntax:
   * <code>spawn &lt; p<sub>1</sub> | ... | p<sub>n</sub> &gt;</code>
   */
  abstract class Spawn {
    def <(p: => Unit): Spawn
    def |(p: => Unit): Spawn
    def > : Unit
  }
  val spawn = new Spawn {
  //object spawn extends Spawn { // BUG !
    def <(p: => Unit): Spawn = { scala.concurrent.ops.spawn(p); this }
    def |(p: => Unit): Spawn = { scala.concurrent.ops.spawn(p); this }
    def > : Unit = ()
  }

  /////////////////////////// GUARDED PROCESSES //////////////////////////

  /** Untyped channel. */
  class UChan {
    /** Default log function. */
    var log = (x: Any) => ()
  }

  /** An untyped guarded process.
   *
   *  @param n         channel name
   *  @param polarity  input (true) or output (false)
   *  @param v         transmitted value
   *  @param c         continuation
   */
  case class UGP(n: UChan, polarity: Boolean, v: Any, c: Any => Any)

  /** Typed guarded process. */
  class GP[a](n: UChan, polarity: Boolean, v: Any, c: Any => a) {
    val untyped = UGP(n, polarity, v, c)
  }

  //////////////////////////////// CHANNELS //////////////////////////////

  /**
   * Name on which one can emit, receive or that can be emitted or received
   * during a communication.
   */
  class Chan[A] extends UChan with Function1[A, Product[A]] {

    var defaultValue: A = _

    /** Creates an input guarded process. */
    def input[B](c: A => B) =
      new GP(this, true, (), x => c(x.asInstanceOf[A]))

    /** Creates an input guarded process. */
    def output[B](v: A, c: () => B) =
      new GP(this, false, v, x => c())

    /** Blocking read. */
    def read = {
      var res: A = defaultValue
      choice ( input(x => res = x) )
      res
    }

    /** Blocking write. */
    def write(x: A) =
      choice ( output(x, () => ()) )

    /** Syntactic sugar for input. */
    def *[B](f: A => B) =
      input(f)

    /** Syntactic sugar for output. */
    def apply(v: A) =
      new Product(this, v)

    /** Attach a function to be evaluated at each communication event
     *  on this channel. Replace previous attached function.
     */
    def attach(f: A => Unit) =
      log = x => f(x.asInstanceOf[A])
  }

  class Product[A](c: Chan[A], v: A) {
    def *[B](f: => B) = c.output(v, () => f)
  }

  /////////////////////// SUM OF GUARDED PROCESSES ///////////////////////

  case class Sum(gs: List[UGP]) {

    /** Continuation of the sum. */
    var cont: () => Any = _

    var initialized = false

    /** Block if not initialized otherwise continue with the
     *  continuation.
     */
    def continue = synchronized {
      if (!initialized) wait()
      cont()
    }

    /** Set the values of parameters and awake the sleeping sum.
     *
     *  @param f ...
     */
    def set(f: () => Any) = synchronized {
      cont = f
      initialized = true
      notify()
    }
  }

  ///////////////////////////// COMMUNICATION ////////////////////////////

  private var sums: List[Sum] = Nil

  /** Test if two lists of guarded processes can communicate.
   *
   *  @param gs1 ...
   *  @param gs2 ...
   *  @return    ...
   */
  private def matches(gs1: List[UGP], gs2: List[UGP]): Option[(() => Unit, () => Any, () => Any)] =
    (gs1, gs2) match {
      case (Nil, _) => None
      case (_, Nil) => None
      case (UGP(a1, d1, v1, c1) :: rest1, UGP(a2, d2, v2, c2) :: rest2) =>
        if (a1 == a2 && d1 == !d2)
          Some(((() => if (d1) a1.log(v2) else a1.log(v1)), (() => c1(v2)), (() => c2(v1))))
        else matches(gs1, rest2) match {
          case None => matches(rest1, gs2)
          case Some(t) => Some(t)
        }
    }

  /** Test if the given sum can react with one of the pending sums.
   *  If yes then do the reaction otherwise append the sum at the end
   *  of the pending sums.
   *
   *  @param s1 ...
   *  @param ss ...
   *  @return   ...
   */
  private def compare(s1: Sum, ss: List[Sum]): List[Sum] =
    ss match {
      case Nil => ss ::: List(s1)
      case s2 :: rest => matches(s1.gs, s2.gs) match {
        case None => s2 :: compare(s1, rest)
        case Some((log, c1, c2)) =>
          log()
          s1.set(c1)
          s2.set(c2)
          rest
      }
    }

  /** Pi-calculus non-deterministic choice.
   *
   *  @param s ...
   *  @return  ...
   */
  def choice[A](s: GP[A]*): A = {
    val sum = Sum(s.toList map { _.untyped })
    synchronized { sums = compare(sum, sums) }
    (sum.continue).asInstanceOf[A]
  }

}
