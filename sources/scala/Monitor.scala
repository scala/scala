package scala with {

  class Monitor extends NativeMonitor() with {

    def synchronized[a](def p: a): a = {
      var value: Ref[a] = null;
      synchronised(=> value = Ref(p));
      value.elem
    }

    def await(def cond: Boolean) = while (!cond) { this.wait() }

  }

}
