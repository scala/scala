package scala.concurrent;

class Lock with Monitor {
  var available = true;
  def acquire = synchronized {
    if (!available) wait();
    available = false
  }
  def release = synchronized {
    available = true;
    notify()
  }
}

