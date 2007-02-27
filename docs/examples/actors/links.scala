package examples.actors

import scala.actors.Actor
import scala.actors.Actor._

object links extends Application {

  case object Stop

  actor {
    val start = link(p(2))
    start ! Stop
  }

  def p(n: int): Actor =
    if (n == 0) top1()
    else top(p(n-1), n)

  def top(a: Actor, n: int): Actor = actor {
    Console.println("starting actor " + n + " (" + Thread.currentThread() + ")")
    self.trapExit = true
    link(a)
    loop {
      receive {
        case ex @ ('EXIT, from, reason) =>
          Console.println("Actor " + n + " received " + ex)
          exit('finished)
        case any => {
          Console.println("Actor " + n + " received " + any)
          a ! any
        }
      }
    }
  }

  def top1(): Actor = actor {
    Console.println("starting last actor"  + " (" + Thread.currentThread() + ")")
    receive {
      case Stop =>
        Console.println("Last actor now exiting")
        exit('abnormal)
      case any =>
        Console.println("Last actor received " + any)
        top1()
    }
  }
}
