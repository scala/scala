package scala.concurrent;

class Channel[a] with Monitor {
  private var written = new LinkedList[a];
  private var lastWritten = written;
  private var nreaders = 0;

  def write(x: a) = synchronized {
    lastWritten.next = new LinkedList[a];
    lastWritten = lastWritten.next;
    lastWritten.elem = x;
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
