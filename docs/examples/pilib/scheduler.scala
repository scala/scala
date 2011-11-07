package examples.pilib

import scala.concurrent.pilib._

object scheduler {

  /**
   * Random number generator.
   */
  val random = new util.Random()

  //***************** Scheduler ******************//

  /**
   * A cell of the scheduler whose attached agent is allowed to start.
   */
  def A(a: Chan[Unit], b: Chan[Unit])(d: Chan[Unit], c: Chan[Unit]) {
    ///- ... complete here ...
    choice ( a * { x => C(a, b)(d, c) })
    ///+
  }

  /**
   * A cell of the scheduler in another intermediate state.
   */
  def C(a: Chan[Unit], b: Chan[Unit])(d: Chan[Unit], c: Chan[Unit]) {
    ///- ... complete here ...
    choice (c * { x => B(a, b)(d, c) })
    ///+
  }

  /**
   * A cell of the scheduler whose attached agent is allowed to finish.
   */
  def B(a: Chan[Unit], b: Chan[Unit])(d: Chan[Unit], c: Chan[Unit]) {
    ///- ... complete here ...
    //     choice (b * { x => D(a, b)(d, c) }) // incorrect naive solution
    choice (
      b * { x => choice ( d(()) * A(a, b)(d, c) ) }, // b.'d.A
      d(()) * (choice (b * { x => A(a, b)(d, c) }))  // 'd.b.A
    )
    ///+
   }

  /**
   * A cell of the scheduler whose attached agent is not yet allowed to start.
   */
  def D(a: Chan[Unit], b: Chan[Unit])(d: Chan[Unit], c: Chan[Unit]) {
    ///- ... complete here ...
    choice (d(()) * A(a, b)(d, c))
    ///+
  }

  //***************** Agents ******************//

  def agent(i: Int)(a: Chan[Unit], b: Chan[Unit]) {
    // 50% chance that we sleep forever
    if (i == 0 && random.nextInt(10) < 5) {
      a.attach(x => println("Start and sleeps ----> " + i))
      Thread.sleep(random.nextInt(1000))
      a.write(())
    }
    else {
      a.attach(x => println("Start ----> " + i))
      b.attach(x => println("Stop -> " + i))
      Thread.sleep(random.nextInt(1000))
      a.write(())
      Thread.sleep(random.nextInt(1000))
      b.write(())
      agent(i)(a, b)
    }
  }

  //***************** Entry function ******************//

  /**
   * Creates a scheduler for five agents (programs).
   */

  def main(args: Array[String]) {
    val agentNb = 5
    val agents = List.range(0, agentNb) map agent
    scheduleAgents(agents)
  }

  //***************** Infrastructure *****************//

  /**
   * A cell is modelled as a function that takes as parameters
   * input and output channels and which returns nothing.  
   */
  type Cell = (Chan[Unit], Chan[Unit]) => Unit

  /**
   * Creates a cell composed of two cells linked together.
   */
  def join(cell1: Cell, cell2: Cell): Cell =
    (l: Chan[Unit], r: Chan[Unit]) => {
      val link = new Chan[Unit];
      spawn < cell1(l, link) | cell2(link, r) >
    };

  /**
   * Links the output of a cell to its input.
   */
  def close(cell: Cell) {
    val a = new Chan[Unit]
    cell(a, a)
  }

  /**
   * Creates a cell consisting of a chain of cells.
   */
  def chain(cells: List[Cell]): Cell =
    cells reduceLeft join

  /**
   * Creates a cell consisting of a chain of cells.
   */
  def makeRing(cells: List[Cell]): Unit =
    close(chain(cells))

  /**
   * An agent is modelled as a function that takes as parameters channels to
   * signal that it has started or finished.
   */
  type Agent = (Chan[Unit], Chan[Unit]) => Unit

  /**
   * Takes a list of agents and schedules them.
   */
  def scheduleAgents(agents: List[Agent]) {
    var firstAgent = true;
    val cells = agents map (ag => {
      val a = new Chan[Unit];
      val b = new Chan[Unit];
      spawn < ag(a, b) >;
      (d: Chan[Unit], c: Chan[Unit]) => if (firstAgent) {
        firstAgent = false;
        A(a, b)(d, c)
      }
      else
        D(a, b)(d, c)
    });
    makeRing(cells)
  }

}


