package examples.pilib

import scala.concurrent.pilib._

/** Two-place buffer specification and implementation. */
object twoPlaceBuffer extends Application {

  /**
   * Specification.
   */
  def Spec[a](in: Chan[a], out: Chan[a]): Unit = {

    def B0: unit = choice (
      in * (x => B1(x))
    )

    def B1(x: a): unit = choice (
      out(x) * (B0),
      in * (y => B2(x, y))
    )

    def B2(x: a, y: a): unit = choice (
      out(x) * (B1(y))
    )

    B0
  }

  /**
   * Implementation using two one-place buffers.
   */
  def Impl[a](in: Chan[a], out: Chan[a]): unit =  {
    ///- ... complete here ...
    // one-place buffer
    def OnePlaceBuffer[a](in: Chan[a], out: Chan[a]): Unit = {
      def B0: unit = choice ( in * (x => B1(x)) )
      def B1(x: a): unit = choice ( out(x) * (B0))
      B0
    }
    val hidden = new Chan[a]
    spawn < OnePlaceBuffer(in, hidden) | OnePlaceBuffer(hidden, out) >
    ///+
  }

  val random = new java.util.Random()

  def Producer(n: Int, in: Chan[String]): Unit = {
    Thread.sleep(random.nextInt(1000))
    val msg = "" + n
    choice (in(msg) * {})
    Producer(n + 1, in)
  }

  def Consumer(out: Chan[String]): unit = {
    Thread.sleep(random.nextInt(1000));
    choice (out * { msg => () });
    Consumer(out)
  }

  val in = new Chan[String]
  in.attach(s => System.out.println("put " + s))
  val out = new Chan[String]
  out.attach(s => System.out.println("get " + s))
  //spawn < Producer(0, in) | Consumer(out) | Spec(in, out) >
  spawn < Producer(0, in) | Consumer(out) | Impl(in, out) >

}
