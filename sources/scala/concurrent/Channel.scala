package scala.concurrent;

class Channel[a]() extends Monitor() {
  private var written: LinkedList[a] = new LinkedList();
  private var lastWritten = written;
  private var nreaders = 0;

  def write(x: a) = synchronized {
    lastWritten.next = new LinkedList();
    lastWritten = lastWritten.next;
    if (nreaders > 0) notify();
  }

  def read: a = synchronized {
    if (written.next == null) {
      nreaders = nreaders + 1; wait(); nreaders = nreaders - 1;
    }
    written = written.next;
    written.elem;
  }
}
