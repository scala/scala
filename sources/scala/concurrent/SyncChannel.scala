package scala.concurrent;

class SyncChannel[a]() extends Monitor() {
  private var data: a = _;
  private var reading = False;
  private var writing = False;

  def write(x: a) = synchronized {
    await(!writing);
    data = x;
    writing = True;
    if (reading) notifyAll();
    else await(reading)
  }

  def read: a = synchronized {
    await(!reading);
    reading = True;
    await(writing);
    val x = data;
    writing = False;
    reading = False;
    notifyAll();
    x
  }
}
