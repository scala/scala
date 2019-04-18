import scala.annotation.{tailrec, dropthis}

abstract class LList[+T] {

  // Tail recursive
  @tailrec final def foreach1(g: T => Unit): Unit = {
    if(this != LNil) {
      val l2 = this.asInstanceOf[LCons[T]]
      g(l2.head)
      l2.tail.foreach1(g)
    }
  }

  // Iterative
  def foreach2(g: T => Unit): Unit = {
    var l = this
    while(l != LNil) {
      val l2 = l.asInstanceOf[LCons[T]]
      g(l2.head)
      l = l2.tail
    }
  }

  // Iterative with @dropthis
  def foreach3(g: T => Unit): Unit = {
    var l = this: @dropthis
    while(l != LNil) {
      val l2 = l.asInstanceOf[LCons[T]]
      g(l2.head)
      l = l2.tail
    }
  }
}

final object LNil extends LList[Nothing]

final class LCons[T](val head: T, _tail: () => LList[T]) extends LList[T] { def tail = _tail() }

object Test extends App {
  import scala.util.{Try, Success, Failure}
  import scala.ref.WeakReference
  case object GCSuccess extends RuntimeException

  def mk(i: Int): LList[Int] = i match {
    case 0 => LNil
    case i => new LCons[Int](i, () => mk(i-1))
  }

  def assertDealloc(op: (=> LList[Int], Int => Unit) => Any): Unit = {
    var ll = mk(200)
    val ref = WeakReference(ll)

    def step(n: Int): Unit = {
      ll = null
      System.gc()
      Thread.sleep(10)
      if(ref.get.isEmpty) throw GCSuccess
    }

    Try { op(ref(), step) } match {
      case Failure(GCSuccess) => println("GC success - List deallocated")
      case Failure(t) => throw t
      case Success(_) => println("List not deallocated")
    }
  }

  println("foreach1")
  assertDealloc(_.foreach1(_))

  println("foreach2")
  assertDealloc(_.foreach2(_))

  println("foreach3")
  assertDealloc(_.foreach3(_))
}
