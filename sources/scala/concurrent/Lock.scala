package scala.concurrent;

class Lock() extends Monitor() {
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

