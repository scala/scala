import scala._;

module sorter {

  def sort(a: List[Int]): List[Int] = {
    val pivot = a at (a.length / 2);
    sort(a.filter(x => x < pivot))
      :::  a.filter(x => x == pivot)
      :::  sort(a.filter(x => x > pivot))
  }

}