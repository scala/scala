package scala.concurrent;

class MailBox with Monitor {

  type Message = AnyRef;

  private abstract class Receiver extends Monitor {
    type t;
    val receiver: PartialFunction[Message, t];
    var msg: Message = _;
    def receive(): t = synchronized {
      if (msg != null) wait();
      receiver(msg)
    }
    def receiveWithin(msec: Long): t = synchronized {
      if (msg != null) wait(msec);
      receiver(if (msg != null) msg else TIMEOUT)
    }
  }

  private val sent = new LinkedList[Message];
  private var lastSent = sent;
  private var receivers = new LinkedList[Receiver];
  private var lastReceiver = receivers;

  def send(msg: Message): Unit = synchronized {
    var rs = receivers, rs1 = rs.next;
    while (rs1 != null && !rs1.elem.receiver.isDefinedAt(msg)) {
      rs = rs1; rs1 = rs1.next;
    }
    if (rs1 != null) {
      rs.next = rs1.next; rs1.elem.msg = msg; rs1.elem.notify();
    } else {
      lastSent = lastSent.append(msg)
    }
  }

  def scanSentMsgs[a](r: Receiver { type t = a }): Unit = synchronized {
    var ss = sent, ss1 = ss.next;
    while (ss1 != null && !r.receiver.isDefinedAt(ss1.elem)) {
      ss = ss1; ss1 = ss1.next
    }
    if (ss1 != null) {
      ss.next = ss1.next; r.msg = ss1.elem;
    } else {
      lastReceiver = lastReceiver append r;
    }
  }

  def receive[a](f: PartialFunction[Message, a]): a = {
    val r = new Receiver { type t = a; val receiver = f }
    scanSentMsgs(r);
    r.receive()
  }

  def receiveWithin[a](msec: Long)(f: PartialFunction[Message, a]): a = {
    val r = new Receiver { type t = a; val receiver = f }
    scanSentMsgs(r);
    r.receiveWithin(msec)
  }
}
