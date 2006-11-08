import scala.actors._
import scala.actors.Actor._

abstract class Producer[T] {

  /** A signal that the next value should be produced. */
  private val Next = new Object

  /** A label for an undefined state of the iterators. */
  private val Undefined = new Object

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

  case object Stop
  class StopException extends Throwable

  /** A thread-based coordinator */
  private val coordinator: Actor = actor {
    try {
    while (true) {
      receive {
        case Next =>
          producer ! Next
          reply {
            receive { case x: Option[_] => x }
          }
        case Stop => throw new StopException
      }
    }
    } catch { case _: StopException => }
  }

  private val producer: Actor = actor {
    receive {
      case Next =>
        produceValues
        coordinator ! None
    }
  }
}

object Test extends Application {

  def from(m: int, n: int) = new Producer[int] {
    def produceValues = for (val i <- m until n) produce(i)
  }

  // note that it works from the main thread
  val it = from(1, 10).iterator
  while (it.hasNext) {
    Console.println(it.next)
  }
}

object Test2 extends Application {

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

  Debug.level = 3

  // note that it works from the main thread
  Console.print("PreOrder:")
  for (val x <- new PreOrder(tree).iterator) Console.print(" "+x)
  Console.print("\nPostOrder:")
  for (val x <- new PostOrder(tree).iterator) Console.print(" "+x)
  Console.print("\nInOrder:")
  for (val x <- new InOrder(tree).iterator) Console.print(" "+x)
  Console.print("\n")
}
