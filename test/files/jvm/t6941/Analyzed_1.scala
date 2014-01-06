// this class's bytecode, compiled under -optimize is analyzed by the test
// method a's bytecode should be identical to method b's bytecode
class SameBytecode {
  def a(xs: List[Int]) = xs match {
    case x :: _ => x
  }

  def b(xs: List[Int]) = xs match {
    case xs: ::[Int] => xs.hd$1
  }
}