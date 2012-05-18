/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

private[actors] class MQueueElement[Msg >: Null](val msg: Msg, val session: OutputChannel[Any], var next: MQueueElement[Msg]) {
  def this() = this(null, null, null)
  def this(msg: Msg, session: OutputChannel[Any]) = this(msg, session, null)
}

private[actors] class MQueue[Msg >: Null](protected val label: String) {
  protected var first: MQueueElement[Msg] = null
  protected var last: MQueueElement[Msg] = null  // last eq null iff list is empty
  private var _size = 0

  def size = _size
  final def isEmpty = last eq null

  protected def changeSize(diff: Int) {
    _size += diff
  }

  def prepend(other: MQueue[Msg]) {
    if (!other.isEmpty) {
      other.last.next = first
      first = other.first
    }
  }

  def clear() {
    first = null
    last = null
    _size = 0
  }


  def append(msg: Msg, session: OutputChannel[Any]) {
    changeSize(1) // size always increases by 1
    val el = new MQueueElement(msg, session)

    if (isEmpty) first = el
    else last.next = el

    last = el
  }

  def append(el: MQueueElement[Msg]) {
    changeSize(1) // size always increases by 1

    if (isEmpty) first = el
    else last.next = el

    last = el
  }

  def foreach(f: (Msg, OutputChannel[Any]) => Unit) {
    var curr = first
    while (curr != null) {
      f(curr.msg, curr.session)
      curr = curr.next
    }
  }

  def foreachAppend(target: MQueue[Msg]) {
    var curr = first
    while (curr != null) {
      target.append(curr)
      curr = curr.next
    }
  }

  def foreachDequeue(target: MQueue[Msg]) {
    var curr = first
    while (curr != null) {
      target.append(curr)
      curr = curr.next
    }
    first = null
    last = null
    _size = 0
  }

  def foldLeft[B](z: B)(f: (B, Msg) => B): B = {
    var acc = z
    var curr = first
    while (curr != null) {
      acc = f(acc, curr.msg)
      curr = curr.next
    }
    acc
  }

  /** Returns the n-th message that satisfies the predicate `p`
   *  without removing it.
   */
  def get(n: Int)(p: Msg => Boolean): Option[Msg] = {
    var pos = 0

    def test(msg: Msg): Boolean =
      p(msg) && (pos == n || { pos += 1; false })

    var curr = first
    while (curr != null)
      if (test(curr.msg)) return Some(curr.msg) // early return
      else curr = curr.next

    None
  }

  /** Removes the n-th message that satisfies the predicate <code>p</code>.
   */
  def remove(n: Int)(p: (Msg, OutputChannel[Any]) => Boolean): Option[(Msg, OutputChannel[Any])] =
    removeInternal(n)(p) map (x => (x.msg, x.session))

  /** Extracts the first message that satisfies the predicate `p`
   *  or `'''null'''` if `p` fails for all of them.
   */
  def extractFirst(p: (Msg, OutputChannel[Any]) => Boolean): MQueueElement[Msg] =
    removeInternal(0)(p) orNull

  def extractFirst(pf: PartialFunction[Msg, Any]): MQueueElement[Msg] = {
    if (isEmpty)    // early return
      return null

    // special handling if returning the head
    if (pf.isDefinedAt(first.msg)) {
      val res = first
      first = first.next
      if (res eq last)
        last = null

      changeSize(-1)
      res
    }
    else {
      var curr = first.next   // init to element #2
      var prev = first

      while (curr != null) {
        if (pf.isDefinedAt(curr.msg)) {
          prev.next = curr.next
          if (curr eq last)
            last = prev

          changeSize(-1)
          return curr // early return
        }
        else {
          prev = curr
          curr = curr.next
        }
      }
      // not found
      null
    }
  }

  private def removeInternal(n: Int)(p: (Msg, OutputChannel[Any]) => Boolean): Option[MQueueElement[Msg]] = {
    var pos = 0

    def foundMsg(x: MQueueElement[Msg]) = {
      changeSize(-1)
      Some(x)
    }
    def test(msg: Msg, session: OutputChannel[Any]): Boolean =
      p(msg, session) && (pos == n || { pos += 1 ; false })

    if (isEmpty)    // early return
      return None

    // special handling if returning the head
    if (test(first.msg, first.session)) {
      val res = first
      first = first.next
      if (res eq last)
        last = null

      foundMsg(res)
    }
    else {
      var curr = first.next   // init to element #2
      var prev = first

      while (curr != null) {
        if (test(curr.msg, curr.session)) {
          prev.next = curr.next
          if (curr eq last)
            last = prev

          return foundMsg(curr) // early return
        }
        else {
          prev = curr
          curr = curr.next
        }
      }
      // not found
      None
    }
  }
}

/** Debugging trait.
 */
private[actors] trait MessageQueueTracer extends MQueue[Any]
{
  private val queueNumber = MessageQueueTracer.getQueueNumber

  override def append(msg: Any, session: OutputChannel[Any]) {
    super.append(msg, session)
    printQueue("APPEND %s" format msg)
  }
  override def get(n: Int)(p: Any => Boolean): Option[Any] = {
    val res = super.get(n)(p)
    printQueue("GET %s" format res)
    res
  }
  override def remove(n: Int)(p: (Any, OutputChannel[Any]) => Boolean): Option[(Any, OutputChannel[Any])] = {
    val res = super.remove(n)(p)
    printQueue("REMOVE %s" format res)
    res
  }
  override def extractFirst(p: (Any, OutputChannel[Any]) => Boolean): MQueueElement[Any] = {
    val res = super.extractFirst(p)
    printQueue("EXTRACT_FIRST %s" format res)
    res
  }

  private def printQueue(msg: String) = {
    def firstMsg = if (first eq null) "null" else first.msg
    def lastMsg = if (last eq null) "null" else last.msg

    println("[%s size=%d] [%s] first = %s, last = %s".format(this, size, msg, firstMsg, lastMsg))
  }
  override def toString() = "%s:%d".format(label, queueNumber)
}

private[actors] object MessageQueueTracer {
  // for tracing purposes
  private var queueNumberAssigner = 0
  private def getQueueNumber = synchronized {
    queueNumberAssigner += 1
    queueNumberAssigner
  }
}
