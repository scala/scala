package scala.concurrent;

class SyncChannel[a] {
  private var data: a = _;
  private var reading = false;
  private var writing = false;

  def await(def cond: Boolean) = while (!cond) { wait() }

  def write(x: a) = synchronized {
    await(!writing);
    data = x;
    writing = true;
    if (reading) notifyAll();
    else await(reading)
  }

  def read: a = synchronized {
    await(!reading);
    reading = true;
    await(writing);
    val x = data;
    writing = false;
    reading = false;
    notifyAll();
    x
  }
}
