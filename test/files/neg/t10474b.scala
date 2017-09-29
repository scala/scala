object Crimp {

  def test(pf: PartialFunction[Any, Unit]) = ()

  def wat: Int = 1
  def whut(i: Int): Int = 1

}

object Clamp {
  import Crimp._

  test {
    case Crimp.wat =>
    case Crimp.whut =>
    case `wat` =>
    case `whut` =>

    case Crimp.wat() =>
    case Crimp.whut() =>
    case wat() =>
    case whut() =>

    case Crimp.wat(_) =>
    case Crimp.whut(_) =>
    case wat(_) =>
    case whut(_) =>

    case Crimp.wat.U =>
    case Crimp.whut.U =>
    case wat.U =>
    case whut.U =>
  }

}