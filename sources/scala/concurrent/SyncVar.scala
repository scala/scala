package scala.concurrent;

class SyncVar[a] {
  private var isDefined: Boolean = false;
  private var value: a = _;
  def get = synchronized {
    if (!isDefined) wait();
    value
  }
  def set(x: a) = synchronized {
    value = x ; isDefined = true ; notifyAll();
  }
  def isSet: Boolean =
    isDefined;
  def unset = synchronized {
    isDefined = false;
  }
}

