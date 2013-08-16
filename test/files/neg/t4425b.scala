object Test1 {
  object X { def unapply(x : String)(y: String) = throw new Exception }

  def f1() {
    println(      "" match { case _ X _   => "ok" ; case _ => "fail" })
    println((X: Any) match { case _ X _   => "ok" ; case _ => "fail" })
    println(      "" match { case X(_)    => "ok" ; case _ => "fail" })
    println((X: Any) match { case X(_)    => "ok" ; case _ => "fail" })
    println(      "" match { case X(_, _) => "ok" ; case _ => "fail" })
    println((X: Any) match { case X(_, _) => "ok" ; case _ => "fail" })
  }
}

object Test2 {
  object X { def unapply(x : String) = throw new Exception }

  def f1() {
    println(      "" match { case _ X _   => "ok" ; case _ => "fail" })
    println((X: Any) match { case _ X _   => "ok" ; case _ => "fail" })
    println(      "" match { case X(_)    => "ok" ; case _ => "fail" })
    println((X: Any) match { case X(_)    => "ok" ; case _ => "fail" })
    println(      "" match { case X(_, _) => "ok" ; case _ => "fail" })
    println((X: Any) match { case X(_, _) => "ok" ; case _ => "fail" })
  }
}

object Test3 {
  object X { def unapply(x : String) = None }

  def f1() {
    println(      "" match { case _ X _   => "ok" ; case _ => "fail" })
    println((X: Any) match { case _ X _   => "ok" ; case _ => "fail" })
    println(      "" match { case X(_)    => "ok" ; case _ => "fail" })
    println((X: Any) match { case X(_)    => "ok" ; case _ => "fail" })
    println(      "" match { case X(_, _) => "ok" ; case _ => "fail" })
    println((X: Any) match { case X(_, _) => "ok" ; case _ => "fail" })
  }
}
