package examples.actors

import scala.actors.Actor._
import scala.actors.{Channel, OutputChannel}

/**
 @author Philipp Haller
 @version 1.0, 07/09/2007
 */
object fringe extends Application {

  abstract class Tree
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(v: Int) extends Tree

  val comparator = actor {
    val extractor1 = actor(extractorBehavior())
    val extractor2 = actor(extractorBehavior())
    val ch1 = new Channel[Any]
    val ch2 = new Channel[Any]
    loop {
      react {
        case ('Fringe, tree1, tree2) =>
          extractor1.send(('Fringe, tree1), ch1)
          extractor2.send(('Fringe, tree2), ch2)
          self ! Triple('Equal, ch1.?, ch2.?)

        case ('Equal, atom1, atom2) =>
          println("comparing "+atom1+" and "+atom2)
          if (atom1 == atom2) atom1 match {
            case None =>
              println("same fringe")
              exit()
            case _ =>
              self ! Triple('Equal, ch1.?, ch2.?)
          } else {
            println("fringes differ")
            exit()
          }
      }
    }
  }

  val extractorBehavior = () => {
    var output: OutputChannel[Any] = null
    loop {
      react {
        case ('Fringe, tree) =>
          output = sender
          self ! ('Extract, tree)

        case ('Extract, tree) => tree match {
          case atom @ Leaf(_) =>
            println("sending "+Some(atom))
            output ! Some(atom)
            sender ! 'Continue

          case Node(left, right) =>
            val outer = self
            val outerCont = sender
            val cont = actor {
              react {
                case 'Continue =>
                  outer.send(('Extract, right), outerCont)
              }
            }
            self.send(('Extract, left), cont)
        }

        case 'Continue =>
          output ! None
          exit()
      }
    }
  }

  comparator ! ('Fringe, Node(Leaf(5), Node(Leaf(7), Leaf(3))),
                Node(Leaf(5), Node(Leaf(7), Leaf(3))))
}
