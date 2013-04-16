// method a's bytecode should be identical to method b's bytecode under -optimize
final class SameBytecode {
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
