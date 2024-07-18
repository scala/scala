//> using options -Ycheck:extmethods -Xfatal-warnings
//
object O {
  implicit class Foo[A](val value: String) extends AnyVal {
    def bippy() = {
      @annotation.tailrec def loop(x: A): Unit = loop(x)
      ()
    }

    // The original test cases fail with the new is/asInstanceOf semantics
    // introduced along with SIP-23 implementation because the method has a
    // singleton typed argument which cannot be erased correctly.
    // See: neg/sip23-tailrec-value-class.scala
    //def boppy() = {
    //  @annotation.tailrec def loop(x: value.type): Unit = loop(x)
    //  ()
    //}

    //def beppy[C](c: => C) = {
    //  () => c
    //  @annotation.tailrec def loop(x: value.type): Unit = loop(x)
    //    () => c
    //  ()
    //}

    def boppy() = {
      @annotation.tailrec def loop(x: Option[value.type]): Unit = loop(x)
      ()
    }

    def beppy[C](c: => C) = {
      () => c
      @annotation.tailrec def loop(x: Option[value.type]): Unit = loop(x)
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
