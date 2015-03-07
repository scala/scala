// method a's bytecode should be identical to method b's bytecode under -optimize
final class Simple { // also covers calling the apply method on Tuple2, as that's what this desugars to
  def a(x1: Any, x2: Any): Int = (x1, x2) match {
    case (s1: String, s2: String) => return 1
    case (i1: Int, s2)            => return 2
    case (s1, s2: String)         => return 3
    case _                        => return 4
  }

  def b(x1: Any, x2: Any): Int = {
    if (x1.isInstanceOf[String] && x2.isInstanceOf[String]) return 1
    else if (x1.isInstanceOf[Int])                          return 2
    else if (x2.isInstanceOf[String])                       return 3
    else                                                    return 4
  }
}


final class Constructor {
  def a(x1: Any, x2: Any): Int = ((new Tuple2(x1, x2))) match {
    case (s1: String, s2: String) => return 1
    case (i1: Int, s2)            => return 2
    case (s1, s2: String)         => return 3
    case _                        => return 4
  }

  def b(x1: Any, x2: Any): Int = {
    if (x1.isInstanceOf[String] && x2.isInstanceOf[String]) return 1
    else if (x1.isInstanceOf[Int])                          return 2
    else if (x2.isInstanceOf[String])                       return 3
    else                                                    return 4
  }
}

// should not optimize
final class Referenced {
  def a(x1: Any, x2: Any): Boolean = (x1, x2) match {
    case t@(s1, s2) => t.isInstanceOf[Tuple2[_, _]]
  }

  def b(x1: Any, x2: Any): Boolean = {
    val scrut = (x1, x2)
    scrut.isInstanceOf[Tuple2[_, _]]
  }
}

// should not optimize
final class Sneaky {
  def a(x1: Any, x2: Any): Int = {???; Tuple2}.apply(x1, x2) match {
    case (s1: String, s2: String) => return 1
    case (i1: Int, s2)            => return 2
    case (s1, s2: String)         => return 3
    case _                        => return 4
  }

  def b(x1: Any, x2: Any): Int = {
    val scrut = {???; Tuple2}.apply(x1, x2) // should not elide the ??? call
    if (scrut._1.isInstanceOf[String] && scrut._2.isInstanceOf[String]) return 1
    else if (scrut._1.isInstanceOf[Int])                          return 2
    else if (scrut._2.isInstanceOf[String])                       return 3
    else                                                    return 4
  }
}
