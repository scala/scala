package scala.concurrent;

class Lock() extends Monitor() {
  var available = True;
  def acquire = {
    if (!available) wait();
    available = False
  }
  def release = {
    available = True;
    notify()
  }
}

