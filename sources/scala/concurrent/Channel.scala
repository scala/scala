package scala.concurrent;

class Channel[a] with Monitor {
  class LinkedList[a] {
    var elem: a = _;
    var next: LinkedList[a] = null;
  }
  private var written = new LinkedList[a];
  private var lastWritten = new LinkedList[a];
  private var nreaders = 0;

  def write(x: a) = synchronized {
    lastWritten.elem = x;
    lastWritten.next = new LinkedList[a];
    lastWritten = lastWritten.next;
    if (nreaders > 0) notify();
  }

  def read: a = synchronized {
    if (written.next == null) {
      nreaders = nreaders + 1; wait(); nreaders = nreaders - 1;
    }
    val x = written.elem;
    written = written.next;
    x
  }
}
