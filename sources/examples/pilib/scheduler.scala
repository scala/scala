object scheduler {

  import scala.concurrent.pilib._;

  /**
  * Random number generator.
  */
  val random = new java.util.Random();

  //***************** Scheduler ******************//

  /**
  * A cell of the scheduler whose attached agent is allowed to start.
  */
  def A(a: Chan[unit], b: Chan[unit])(d: Chan[unit], c: Chan[unit]): unit = {
    a.read;
    C(a,b)(d,c)
  }

  /**
  * A cell of the scheduler in an intermediate state (incorrect)
  */
//   def B(a: Chan[unit], b: Chan[unit])(d: Chan[unit], c: Chan[unit]): unit = {
//     b.read;
//     D(a,b)(d,c)
//   }

  /**
  * A cell of the scheduler in an intermediate state (correct).
  */
  def B(a: Chan[unit], b: Chan[unit])(d: Chan[unit], c: Chan[unit]): unit =
    choice (
      b * (x => D(a,b)(d,c)),
      d(()) * ({ b.read; A(a,b)(d,c)})
    );

  /**
  * A cell of the scheduler in another intermediate state.
  */
  def C(a: Chan[unit], b: Chan[unit])(d: Chan[unit], c: Chan[unit]): unit = {
    c.read;
    B(a,b)(d,c)
  }

  /**
  * A cell of the scheduler whose attached agent is not yet allowed to start.
  */
  def D(a: Chan[unit], b: Chan[unit])(d: Chan[unit], c: Chan[unit]): unit = {
    d.write(());
    A(a,b)(d,c)
  }

  //***************** Agents ******************//

  def agent(i: Int)(a: Chan[unit], b: Chan[unit]): unit = {
    Thread.sleep(random.nextInt(1000) + 1);
    a.write(());
    System.out.println("Starting agent " + i);
    Thread.sleep(random.nextInt(1000) + 1);

    // 10% chance that we sleep for a long while.
    if(random.nextInt(10) == 0) {
      System.out.println("Agent " + i + " sleeps");
      Thread.sleep(20000);
    }

    b.write(());
    System.out.println("Ending agent " + i);
    agent(i)(a, b)
  }

  //***************** Entry function ******************//

  /**
  * Creates a scheduler for five agents (programs).
  */

  def main(args: Array[String]): unit = {
    val agentNb = 5;
    val agents = for(val i <- List.range(0, agentNb)) yield agent(i);
    scheduleAgents(agents);
  }

  //***************** Infrastructure *****************//

  /**
  * A cell is modelled as a function that takes as parameters
  * input and output channels and which returns nothing.
  */
  type Cell = (Chan[unit], Chan[unit]) => unit;

  /**
  * Creates a cell composed of two cells linked together.
  */
  def join(cell1: Cell, cell2: Cell): Cell =
    (l: Chan[unit], r: Chan[unit]) => {
      val link = new Chan[unit];
      spawn < cell1(l, link) | cell2(link, r) >
    };

  /**
  * Links the output of a cell to its input.
  */
  def close(cell: Cell): unit = {
    val a = new Chan[unit];
    cell(a, a)
  }

  /**
  * Creates a cell consisting of a chain of cells.
  */
  def chain(cells: List[Cell]): Cell =
    cells reduceLeft join;

  /**
  * Creates a cell consisting of a chain of cells.
  */
  def makeRing(cells: List[Cell]): unit =
    close(chain(cells));

  /**
  * An agent is modelled as a function that takes as parameters channels to
  * signal that it has started or finished.
  */
  type Agent = (Chan[unit], Chan[unit]) => unit;

  /**
  * Takes a list of agents and schedules them.
  */
  def scheduleAgents(agents: List[Agent]): unit = {
    var firstAgent = true;
    val cells = agents map (ag => {
      val a = new Chan[unit];
      val b = new Chan[unit];
      spawn < ag(a, b) >;
      if (firstAgent) {
	firstAgent = false;
	A(a, b)
      }
      else
	D(a, b)
    });
    makeRing(cells)
  }
}


