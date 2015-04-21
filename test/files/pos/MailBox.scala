package test;

object TIMEOUT

class MailBox {

  private class LinkedList[a] {
    var elem: a = _;
    var next: LinkedList[a] = null;
  }

  private def insert[a](l: LinkedList[a], x: a): LinkedList[a] = {
    l.next = new LinkedList[a];
    l.next.elem = x;
    l.next.next = l.next;
    l
  }

  private abstract class Receiver {
    def isDefined(msg: Any): Boolean;
    var msg: Any = null;
  }

  private val sent = new LinkedList[Any];
  private var lastSent = sent;
  private val receivers = new LinkedList[Receiver];
  private var lastReceiver = receivers;

  def send(msg: Any): Unit = synchronized {
    var r = receivers;
    var r1 = r.next;
    while (r1 != null && !r1.elem.isDefined(msg)) {
      r = r1; r1 = r1.next;
    }
    if (r1 != null) {
      r.next = r1.next; r1.elem.msg = msg; r1.elem.notify();
    } else {
      lastSent = insert(lastSent, msg);
    }
  }

  def receive[a](f: PartialFunction[Any, a]): a = {
    val msg: Any = synchronized {
      var s = sent;
      var s1 = s.next;
      while (s1 != null && !f.isDefinedAt(s1.elem)) {
        s = s1; s1 = s1.next
      }
      if (s1 != null) {
        s.next = s1.next; s1.elem
      } else {
	val r = insert(lastReceiver, new Receiver {
          def isDefined(msg: Any) = f.isDefinedAt(msg);
        });
	lastReceiver = r;
        r.elem.wait();
        r.elem.msg
      }
    }
    f(msg)
  }

  def receiveWithin[a](msec: Long)(f: PartialFunction[Any, a]): a = {
    val msg: Any = synchronized {
      var s = sent;
      var s1 = s.next;
      while (s1 != null && !f.isDefinedAt(s1.elem)) {
        s = s1; s1 = s1.next ;
      }
      if (s1 != null) {
        s.next = s1.next; s1.elem
      } else {
        val r = insert(lastReceiver, new Receiver {
            def isDefined(msg: Any) = f.isDefinedAt(msg);
        });
        lastReceiver = r;
        r.elem.wait(msec);
        if (r.elem.msg == null) r.elem.msg = TIMEOUT;
        r.elem.msg
      }
    }
    f(msg)
  }
}
