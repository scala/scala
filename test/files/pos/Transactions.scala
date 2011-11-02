package scala.concurrent

class AbortException extends RuntimeException

object Transaction {
  private var cnt = 0L
  def nextId: Long = synchronized {
    cnt += 1; cnt
  }

  // Transaction status constants
  val Running = 0
  val Committed = 1
  val Abortable = 2
  val Aborted = 3
  val Compound = 4

  def atomic[T](b: Transaction => T): Option[T] =
    (new Transaction).run(b)
}

class Transaction {
  var status: Int = _

  var id: Long = _  // only for real transactions

  var head: Transaction = this
  var next: Transaction = null

  def this(hd: Transaction, tl: Transaction) = { this(); this.head = head; this.next = next }
  
  def makeAbort() = synchronized {
    while (status != Transaction.Aborted && status != Transaction.Committed) {
      status = Transaction.Abortable
      wait()
    }
  }
  private def abort() = synchronized { status = Transaction.Aborted; notifyAll() }
  private def commit() = synchronized { status = Transaction.Committed; notifyAll() }
  def run[T](b: Transaction => T): Option[T] =
    try {
      status = Transaction.Running
      id = Transaction.nextId
      val result = Some(b(this))
      commit()
      result
    } catch {
      case ex: AbortException => abort(); None
      case ex: Throwable => abort(); throw ex
    }
  
}

trait Transactional {

  /** create a new snapshot */
  def checkPoint(): Unit

  /** copy back snapshot */
  def rollBack(): Unit
  
  var readers: Transaction
  var writer: Transaction

  def currentWriter(): Transaction = null
    if (writer == null) null
    else if (writer.status == Transaction.Running) writer
    else {
      if (writer.status != Transaction.Committed) rollBack(); 
      writer = null; 
      null 
    }
  
  def getter(thisTrans: Transaction) {
    if (writer == thisTrans) return
    var r = readers
    while (r != null && r.head.status != Transaction.Running) { r = r.next; readers = r }
    while (r != null) {
      if (r.head == thisTrans) return
      val last = r
      r = r.next
      while (r != null && r.head.status != Transaction.Running) { r = r.next; last.next = r }
    }
    synchronized {
      if (thisTrans.status == Transaction.Abortable) throw new AbortException
      val w = currentWriter()
      if (w != null)
        if (thisTrans.id < w.id) { w.makeAbort(); rollBack(); writer = null }
        else throw new AbortException
      readers = if (readers == null) thisTrans else new Transaction(thisTrans, readers)
    }
  }

  def setter(thisTrans: Transaction) {
    if (writer == thisTrans) return
    synchronized {
      val w = currentWriter()
      if (w != null)
        if (thisTrans.id < w.id) { w.makeAbort(); rollBack() } 
        else throw new AbortException
      var r = readers
      while (r != null && r.head.status != Transaction.Running) { r = r.next; readers = r }
      while (r != null) {
        if (r.id < thisTrans.id) throw new AbortException
        else w.makeAbort()
        val last = r
        r = r.next
        while (r != null && r.head.status != Transaction.Running) { r = r.next; last.next = r }
      }
      checkPoint()
    }
  }
}
      
