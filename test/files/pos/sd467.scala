import scala.annotation.tailrec

class TestA {
  @tailrec
  final def loop0(i: Int): this.type = {
    if(i == 0) this
    else loop0(i-1)
  }

  @tailrec
  final def loop1(i: Int, self: this.type): Int = {
    if(i == 0) 0
    else loop1(i-1, this)
  }

  @tailrec
  final def loop2(i: Int, self: this.type): this.type = {
    if(i == 0) this
    else loop2(i-1, this)
  }
}

object TestB {
  object Done

  @tailrec
  def loop0(i: Int): Done.type = {
    if(i == 0) Done
    else loop0(i-1)
  }

  @tailrec
  def loop1(i: Int, done: Done.type): Int = {
    if(i == 0) 0
    else loop1(i-1, Done)
  }

  @tailrec
  def loop2(i: Int, done: Done.type): Done.type = {
    if(i == 0) done
    else loop2(i-1, done)
  }
}

object TestC {
  @tailrec
  def loop0(i: Int): 0 = {
    if(i == 0) 0
    else loop0(i-1)
  }

  @tailrec
  def loop1(i: Int, zero: 0): Int = {
    if(i == 0) 0
    else loop1(i-1, 0)
  }

  @tailrec
  def loop2(i: Int, zero: 0): 0 = {
    if(i == 0) 0
    else loop2(i-1, 0)
  }
}
