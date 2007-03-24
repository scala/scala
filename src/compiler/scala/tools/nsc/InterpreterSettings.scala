package scala.tools.nsc

/** Settings for the interpreter */
class InterpreterSettings {
  /** A list of paths where :load should look */
  var loadPath = List(".", "/home/lex/tmp")  // XXX remove tmp, just for testing

  /** The maximum length of toString to use when printing the result
   *  of an evaluation.  0 means no maximum.  If a printout requires
   *  more than this number of characters, then the printout is
   *  truncated.
   */
  var maxPrintString = 390

  override def toString =
    "InterpreterSettings {\n" +
//    "  loadPath = " + loadPath + "\n" +
    "  maxPrintString = " + maxPrintString + "\n" +
    "}"
}
