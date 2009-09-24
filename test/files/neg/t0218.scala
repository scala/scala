trait APQ {
  class Placement {
  }

  type P <: Placement

  type PP = P

  def pq(numQueens: int, numRows: int) : List[Placement] = {
    List(new PP)
  }
}
