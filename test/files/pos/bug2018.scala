class A {
  val b = new B

  def getChildren = List(new A).iterator

  class B {
    private def check = true

    private def getAncestor(p: A): A = {
      val c = (p.getChildren.find(_.b.check)) match {case Some(d) => d case None => p}

      if (c == p) p else c.b.getAncestor(c)
    }
  }
}