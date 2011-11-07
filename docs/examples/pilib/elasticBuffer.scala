package examples.pilib

object elasticBuffer {

  import scala.concurrent.pilib._

  /**
   * Recursive type for channels that carry a "String" channel and
   * an object of the type we define.
   */
  class MetaChan extends Chan[Pair[Chan[String], MetaChan]]

  def Buffer(put: Chan[String], get: Chan[String]): Unit = {

    /**
     * An empty buffer cell, ready to pass on (o,r) to the left.
     */
    def Bl(i:Chan[String], l: MetaChan,
           o: Chan[String], r: MetaChan): unit =
      choice (
        l(Pair(o,r)) * (System.out.println("Removed one cell.")),
        i * (inp => Cl(i, l, o, r, inp))
      )

    /**
     * A buffer cell containing a value, ready to receive (o,r) from the right.
     */
    def Cl(i: Chan[String], l: MetaChan, 
           o: Chan[String], r: MetaChan, content: String): Unit =
      choice (
        o(content) * (Bl(i,l,o,r)),
        i * (inp => Dl(i,l,o,r,content, inp)),
        r * ( { case Pair(newo, newr) => Cl(i,l,newo,newr,content) })
      )

    /**
     * Two joined buffer cells, of type Cl
     */
    def Dl(i: Chan[String], l: MetaChan,
           o: Chan[String], r: MetaChan,
           content: String, inp: String): Unit = {
      val newlr = new MetaChan
      val newio = new Chan[String]
      spawn < Cl(i, l, newio, newlr, inp) | Cl(newio, newlr, o, r, content) >
    }

    // l and r channels for the leftmost and rightmost cell, respectively.
    val unused1 = new MetaChan
    val unused2 = new MetaChan

    Bl(put, unused1, get, unused2)
  }

  val random = new java.util.Random()

  def Producer(n: int, put: Chan[String]): Unit = {
    Thread.sleep(1 + random.nextInt(1000))
    val msg = "object " + n
    put.write(msg)
    System.out.println("Producer gave " + msg)
    Producer(n + 1, put)
  }

  def Consumer(get: Chan[String]): Unit = {
    Thread.sleep(1 + random.nextInt(1000))
    val msg = get.read
    System.out.println("Consumer took " + msg)
    Consumer(get)
  }

  def main(args: Array[String]): Unit = {
    val put = new Chan[String]
    val get = new Chan[String]
    spawn < Producer(0, put) | Consumer(get) | Buffer(put, get) >
  }

}
