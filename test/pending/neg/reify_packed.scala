object Test extends App {
  reify {
    class C { override def toString() = "C" }
    val ret = List((new C, new C))
    ret.asInstanceOf[List[_]]
  };

  val toolbox = mkToolBox()
  println(toolbox.runExpr(code.tree))
}
