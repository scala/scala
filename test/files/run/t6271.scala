object Test extends App {
  def filterIssue = {
    val viewed : Iterable[Iterable[Int]] = List(List(0).view).view
    val filtered = viewed flatMap { x => List( x filter (_ > 0) ) }
    filtered.iterator.to(Iterable).flatten
  }
  def takenIssue = {
    val viewed : Iterable[Iterable[Int]] = List(List(0).view).view
    val filtered = viewed flatMap { x => List( x take 0 ) }
    filtered.iterator.to(Iterable).flatten
  }
  def droppedIssue = {
    val viewed : Iterable[Iterable[Int]] = List(List(0).view).view
    val filtered = viewed flatMap { x => List( x drop 1 ) }
    filtered.iterator.to(Iterable).flatten
  }
  def flatMappedIssue = {
    val viewed : Iterable[Iterable[Int]] = List(List(0).view).view
    val filtered = viewed flatMap { x => List( x flatMap (_ => List()) ) }
    filtered.iterator.to(Iterable).flatten
  }
  def slicedIssue = {
    val viewed : Iterable[Iterable[Int]] = List(List(0).view).view
    val filtered = viewed flatMap { x => List( x slice (2,3) ) }
    filtered.iterator.to(Iterable).flatten
  }
  filterIssue
  takenIssue
  droppedIssue
  flatMappedIssue
  slicedIssue
}
