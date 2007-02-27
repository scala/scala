package examples.actors

import scala.actors.Actor
import scala.actors.Actor._

abstract class Producer[T] {

  /** A signal that the next value should be produced. */
  private val Next = new Object

  /** A label for an undefined state of the iterators. */
  private val Undefined = new Object

  /** A signal to stop the coordinator. */
  private val Stop = new Object

  protected def produce(x: T): unit = {
    coordinator ! Some(x)
    receive { case Next => }
  }
  protected def produceValues: unit

  def iterator = new Iterator[T] {
    private var current: Any = Undefined
    private def lookAhead = {
      if (current == Undefined) current = coordinator !? Next
      current
    }

    def hasNext: boolean = lookAhead match {
      case Some(x) => true
      case None => { coordinator ! Stop; false }
    }

    def next: T = lookAhead match {
      case Some(x) => current = Undefined; x.asInstanceOf[T]
    }
  }

  private val coordinator: Actor = actor {
    loop {
      react {
        case Next =>
          producer ! Next
          reply {
            receive { case x: Option[_] => x }
          }
        case Stop => exit('stop)
      }
    }
  }

  private val producer: Actor = actor {
    receive {
      case Next =>
        produceValues
        coordinator ! None
    }
  }
}

object producers extends Application {

  class Tree(val left: Tree, val elem: int, val right: Tree)
  def node(left: Tree, elem: int, right: Tree): Tree = new Tree(left, elem, right)
  def node(elem: int): Tree = node(null, elem, null)

  def tree = node(node(node(3), 4, node(6)), 8, node(node(9), 10, node(11)))

  class PreOrder(n: Tree) extends Producer[int] {
    def produceValues = traverse(n)
    def traverse(n: Tree) {
      if (n != null) {
        produce(n.elem)
        traverse(n.left)
        traverse(n.right)
      }
    }
  }

  class PostOrder(n: Tree) extends Producer[int] {
    def produceValues = traverse(n)
    def traverse(n: Tree) {
      if (n != null) {
        traverse(n.left)
        traverse(n.right)
        produce(n.elem)
      }
    }
  }

  class InOrder(n: Tree) extends Producer[int] {
    def produceValues = traverse(n)
    def traverse(n: Tree) {
      if (n != null) {
        traverse(n.left)
        produce(n.elem)
        traverse(n.right)
      }
    }
  }

  actor {
    Console.print("PreOrder:")
    for (val x <- new PreOrder(tree).iterator) Console.print(" "+x)
    Console.print("\nPostOrder:")
    for (val x <- new PostOrder(tree).iterator) Console.print(" "+x)
    Console.print("\nInOrder:")
    for (val x <- new InOrder(tree).iterator) Console.print(" "+x)
    Console.print("\n")
  }
}
