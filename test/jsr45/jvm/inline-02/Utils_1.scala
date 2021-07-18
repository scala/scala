import Tools_1._

object Utils_1 {
  @inline def blackMagic(list: List[Int]): List[Int] = {
    list.map { n =>
      aura(n)
    }
  }
}
