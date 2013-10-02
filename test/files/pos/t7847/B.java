public final class B {
  void blah() {
    Blah x = Blah.apply2(1);
    Blah y = Blah.apply(1);
    Blah z = Blah$.MODULE$.apply(1);

    scala.Option un1 = Blah.unapply(null);
    scala.Option un2 = Blah$.MODULE$.unapply(null);
  }
}
