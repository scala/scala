object O {
  implicit class Foo[A](val value: String) extends AnyVal {
    def bippy() = {
      @annotation.tailrec def loop(x: A): Unit = loop(x)
      ()
    }

    def boppy() = {
      @annotation.tailrec def loop(x: value.type): Unit = loop(x)
      ()
    }

    def beppy[C](c: => C) = {
      () => c
      @annotation.tailrec def loop(x: value.type): Unit = loop(x)
        () => c
      ()
    }
  }
  // uncaught exception during compilation: Types$TypeError("type mismatch;
  //  found   : A(in method bippy$extension)
  //  required: A(in class Foo)") @ scala.tools.nsc.typechecker.Contexts$Context.issueCommon(Contexts.scala:396)
  // error: scala.reflect.internal.Types$TypeError: type mismatch;
  //  found   : A(in method bippy$extension)
  //  required: A(in class Foo)
}
