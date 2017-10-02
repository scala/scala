
object Other {
  def total: (Int*) => Int = _.sum

  def heady[A]: (A*) => A = _.head
}

object TotallyRad {
  def f() = Other.total(1, 2, 3, 4)

  def g() = {
    import Other._
    total(1, 2, 3, 4)
  }
}

object HeadingForTrouble {
  def f() = Other.heady(1, 2, 3)

  def g() = {
    import Other._
    heady(1, 2, 3)
  }
}
