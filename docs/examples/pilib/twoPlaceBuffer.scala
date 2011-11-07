package examples.pilib

import scala.concurrent.pilib._

/** Two-place buffer specification and implementation. */
object twoPlaceBuffer extends Application {

  /**
   * Specification.
   */
  def Spec[A](in: Chan[A], out: Chan[A]) {

    def B0: Unit = choice (
      in * (x => B1(x))
    )

    def B1(x: A): Unit = choice (
      out(x) * (B0),
      in * (y => B2(x, y)) 
    )

    def B2(x: A, y: A): Unit = choice (
      out(x) * (B1(y))
    )

    B0
  }

  /**
   * Implementation using two one-place buffers.
   */
  def Impl[A](in: Chan[A], out: Chan[A]) {
    ///- ... complete here ...
    // one-place buffer
    def OnePlaceBuffer[A](in: Chan[A], out: Chan[A]) {
      def B0: Unit = choice ( in * (x => B1(x)) )
      def B1(x: A): Unit = choice ( out(x) * (B0))
      B0
    }
    val hidden = new Chan[A]
    spawn < OnePlaceBuffer(in, hidden) | OnePlaceBuffer(hidden, out) >
    ///+
  }

  val random = new util.Random()

  def Producer(n: Int, in: Chan[String]) {
    Thread.sleep(random.nextInt(1000))
    val msg = "" + n
    choice (in(msg) * {})
    Producer(n + 1, in)
  }

  def Consumer(out: Chan[String]) {
    Thread.sleep(random.nextInt(1000))
    choice (out * { msg => () })
    Consumer(out)
  }

  val in = new Chan[String]
  in.attach(s => println("put " + s))
  val out = new Chan[String]
  out.attach(s => println("get " + s))
  //spawn < Producer(0, in) | Consumer(out) | Spec(in, out) >
  spawn < Producer(0, in) | Consumer(out) | Impl(in, out) >

}
