package scala;

class Monitor() extends NativeMonitor() with {

  def synchronized[a](def p: a): a = synchronised(p);

  def await(def cond: Boolean) = while (!cond) { this.wait() }

}
