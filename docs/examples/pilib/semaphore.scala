package examples.pilib

import scala.concurrent.pilib._

/** Solution of exercise session 6 (first question). */
object semaphore {

  class Signal extends Chan[Unit] {
    def send = write(())
    def receive = read
  }

  /** Interface. */
  trait Semaphore {
    def get: Unit
    def release: Unit
  }

  /** First implementation. */
  class Sem1 extends Semaphore {

    private val g = new Signal
    private val r = new Signal

    def get: Unit = g.send
    def release: Unit = r.send

    private def Sched: Unit = choice (
      g * (x => { r.receive; Sched }),
      r * (x => Sched)
    )
    spawn< Sched >
  }

  /** Second implementation. */
  class Sem2 extends Semaphore {

    private val a = new Signal
    private val na = new Signal

    def get { a.receive; spawn< na.send > }
    def release: Unit = choice (
      a * (x => spawn< a.send >),
      na * (x => spawn< a.send >)
    )
    spawn< a.send >
  }

  /** Test program. */
  def main(args: Array[String]) {
    val random = new util.Random()
    val sem = new Sem2
    def mutex(p: => Unit) { sem.get; p; sem.release }

    spawn< {
      Thread.sleep(1 + random.nextInt(100));
      mutex( {
        println("a1");
        Thread.sleep(1 + random.nextInt(100));
        println("a2")
      } )
    } | {
      Thread.sleep(1 + random.nextInt(100));
      mutex( {
        println("b1");
        Thread.sleep(1 + random.nextInt(100));
        println("b2")
      } ) 
    } >;
  }
}

