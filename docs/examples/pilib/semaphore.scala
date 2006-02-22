package examples.pilib

/** Solution of exercise session 6 (first question). */
object semaphore {

  import scala.concurrent.pilib._

  class Signal extends Chan[unit] {
    def send = write(())
    def receive = read
  }

  /** Interface. */
  trait Semaphore {
    def get: unit
    def release: unit
  }

  /** First implementation. */
  class Sem1 extends Semaphore {

    private val g = new Signal
    private val r = new Signal

    def get: unit = g.send
    def release: unit = r.send

    private def Sched: unit = choice (
      g * (x => { r.receive; Sched }),
      r * (x => Sched)
    )
    spawn< Sched >
  }

  /** Second implementation. */
  class Sem2 extends Semaphore {

    private val a = new Signal
    private val na = new Signal

    def get: unit = { a.receive; spawn< na.send > }
    def release: unit = choice (
      a * (x => spawn< a.send >),
      na * (x => spawn< a.send >)
    )
    spawn< a.send >
  }

  /** Test program. */
  def main(args: Array[String]): unit = {
    val random = new java.util.Random()
    val sem = new Sem2
    def mutex(p: => unit): unit = { sem.get; p; sem.release }

    spawn< {
      Thread.sleep(1 + random.nextInt(100));
      mutex( {
        System.out.println("a1");
        Thread.sleep(1 + random.nextInt(100));
        System.out.println("a2")
      } )
    } | {
      Thread.sleep(1 + random.nextInt(100));
      mutex( {
        System.out.println("b1");
        Thread.sleep(1 + random.nextInt(100));
        System.out.println("b2")
      } )
    } >;
  }
}

