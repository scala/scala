package scala.concurrent;

class Lock {
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

