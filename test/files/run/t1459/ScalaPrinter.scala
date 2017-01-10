class ScalaPrinter extends VarArg {
  override def doit(s: String*) = {
    print("ScalaPrinter: ")
    s.foreach(print _)
  }
}