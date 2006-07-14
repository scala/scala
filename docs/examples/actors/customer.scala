/**
 * @author Philipp Haller <philipp.haller@epfl.ch>
 *
 * This shows "customer passing" for implementing
 * recursive algorithms using actors.
 */

package examples.actors

import scala.actors.single.Actor

case class Factorial(n: int, resTo: Actor)

class FactorialProcess extends Actor {
  override def run: unit = {
    receive {
      case Factorial(n, resTo) =>
        if (n == 0) {
          resTo ! 1
        }
        else {
          val m = new MultiplyActor(n, resTo)
          m.start()
          this ! Factorial(n-1, m)
        }
        run
    }
  }
}

class MultiplyActor(factor: int, resTo: Actor) extends Actor {
  override def run: unit =
    receive {
      case value: int =>
        resTo ! factor * value
    }
}

object CustomerPassing {
  def main(args: Array[String]): unit = {
    val fac = new FactorialProcess
    fac.start()

    val c = new Actor {
      override def run: unit = {
        fac ! Factorial(3, this)

        receive {
          case value: int =>
            System.out.println("Result: " + value)
        }
      }
    }
    c.start()
  }
}
