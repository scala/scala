module sorter {

def sort (a: List[Int]): List[Int] = {
  val pivot = a at (a.length / 2);
  def leqPivot(x: Int) = x <= pivot;
  def gtPivot(x: Int) = x > pivot;
  def eqPivot(x: Int) = x == pivot;
  sort(a filter leqPivot)
    ::: sort(a filter eqPivot)
    ::: sort(a filter gtPivot)
}
}