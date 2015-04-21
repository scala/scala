class MissingLineNumbers {
  val foo: Class[_] = classOf            // error without position, line or file
  val foo1: Class[_] = classOf[X]        // good error, all info contained
  val foo2 = classOf                     // Infers  T=Nothing

  val foo3: Class[_] = Predef.classOf    // Infers  T=Nothing. Irregular wrt typedIdent.
  val foo4: Class[_] = Predef.classOf[X] // good error, all info contained
  val foo5 = Predef.classOf              // Infers  T=Nothing
}
