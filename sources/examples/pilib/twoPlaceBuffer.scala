/** Two-place buffer specification and implementation. */
object twoPlaceBuffer {

  import scala.concurrent.pilib._;

  /**
  * Specification.
  */
  def Spec[a](put: Chan[a], get: Chan[a]): unit =  {

    def B0: unit = {
      val x = put.read;
      B1(x)
    }

    def B1(x: a): unit = choice (
      get(x) * (B0),
      put * (y => B2(x, y))
    );

    def B2(x: a, y: a): unit = {
      get.write(x);
      B1(y)
    };

    B0
  }

  /**
  * Implementation.
  */
  def Impl[a](put: Chan[a], get: Chan[a]): unit =  {

    // An empty one-place buffer.
    def B0(in: Chan[a], out: Chan[a]): unit = {
      val x = in.read;
      B1(in, out, x)
    }

    // A full one-place buffer containing x.
    def B1(in: Chan[a], out: Chan[a], x: a): unit = {
      out.write(x);
      B0(in, out)
    };

    val hidden = new Chan[a];
    spawn < B0(put, hidden) | B0(hidden, get) >
  }

  val random = new java.util.Random();

  def Producer(n: Int, put: Chan[String]): unit = {
    Thread.sleep(1 + random.nextInt(1000));
    val msg = "object " + n;
    put.write(msg);
    System.out.println("Producer gave " + msg);
    Producer(n + 1, put)
  }

  def Consumer(get: Chan[String]): unit = {
    Thread.sleep(1 + random.nextInt(1000));
    val msg = get.read;
    System.out.println("Consummer took " + msg);
    Consumer(get)
  }

  def main(args: Array[String]): unit = {
    val put = new Chan[String];
    val get = new Chan[String];
    spawn < Producer(0, put) | Consumer(get) | Spec(put, get) >
    //spawn < Producer(0, put) | Consumer(get) | Impl(put, get) >
  }

}
