class C {

  def f[a](x: a): a = {

    class D() {
      def g(x: a) = f(x): a;
    }

    new D().g(x);

  }

}



