// should print HI nine times to indicate the lazy val has been re-initialized on every iteration
object Test extends App {
  def fooDo: Unit = {
    var i = 3
    do {
      lazy val x = { println("HI"); 1 }
      i -= x
    } while(i > 0)
  }

  def fooWhile: Unit = {
    var i = 3
    while(i > 0) {
      lazy val x = { println("HI"); 1 }
      i -= x
    }
  }

  @annotation.tailrec def fooTail(i: Int): Unit = {
    lazy val x = { println("HI"); 1 }
    if (i > 0) fooTail(i - x)
  }


  fooWhile
  fooDo
  fooTail(3)
}
