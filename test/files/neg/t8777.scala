trait Foo extends scala.tools.nsc.Global {
  override def newCodePrinter(out: java.io.PrintWriter, tree: Tree, printRootPkg: Boolean): TreePrinter =
    super.newCodePrinter(out, tree, printRootPkg)
}
