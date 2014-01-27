// Checks that the state of standard implicits in Predef._ and scala._
// doesn't allow us to unambiguously and implicitly convert AnyVal
// and subtypes to AnyRef.
//
// In the days before value classes, this was precariously held be
// the competing implicits Any => StringAdd and Any => StringFormat.
// Since then, these have both become value classes, but seeing as
// this happened simultaneously, we're still okay.
object Test {
  locally {
    1: AnyRef
  }

  locally {
    // before this test case was added and ContextErrors was tweaked, this
    // emitted: "Note that Any extends Any, not AnyRef."
    (null: Any): AnyRef
  }

  locally {
    (0: AnyVal): AnyRef
  }

  class AV(val a: Int) extends AnyVal

  locally {
    new AV(0): AnyRef
  }

  "": AnyVal

  new Object() : AnyVal
}
