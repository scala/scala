import annotation.switch

class switch {
  def test(x: Int) = (x: @switch) match {
    case 1 | 2 | 3 => ()
  }
}