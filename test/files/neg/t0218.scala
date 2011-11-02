trait APQ {  
  class Placement {
  }

  type P <: Placement

  type PP = P

  def pq(numQueens: Int, numRows: Int) : List[Placement] = {
    List(new PP) 
  }
}
