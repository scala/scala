package scala.concurrent;

class Lock with Monitor {
  var available = true;
  def acquire = {
    if (!available) wait();
    available = false
  }
  def release = {
    available = true;
    notify()
  }
}

