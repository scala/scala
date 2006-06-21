/**
 @author Philipp Haller <philipp.haller@epfl.ch>

 This shows "customer passing" for implementing
 recursive algorithms using actors.
 */

import scala.actors._;
import scala.actors.single._;

abstract class FactorialMessage;
case class Factorial(n: int, resTo: Actor) extends FactorialMessage;
case class Value(n: int) extends FactorialMessage;

class FactorialProcess extends Actor {
  override def run: unit = {
    receive {
      case Factorial(n, resTo) =>
        if (n == 0) {
          Debug.info("Sending Value(1) to " + resTo)
          resTo send Value(1)
        }
        else {
          // spawn process that multiplies
          /*val m = spawnReceive({
            case Value(value) => resTo send Value(n * value)
          });*/

          val m = new MultiplyActor(n, resTo)
          m.start
          Debug.info("Sending Factorial(" + (n-1) + ", " + m + ") to " + this)
          this send Factorial(n-1, m)
        }
        run
    }
  }
}

class MultiplyActor(factor: int, resTo: Actor) extends Actor {
  override def run: unit =
    receive {
      case Value(value) =>
        Debug.info("Sending Value(" + factor * value + ") to " + resTo)
        resTo send Value(factor * value)
        Debug.info("Done sending.")
    }
}

object CustomerPassing {
  def main(args: Array[String]): unit = {
    val fac = new FactorialProcess
    fac.start

    val c = new Actor {
      override def run: unit = {
        fac send Factorial(3, this)

        receive {
          case Value(value) =>
            System.out.println("Result: " + value)
        }
      }
    }
    c.start
  }
}
