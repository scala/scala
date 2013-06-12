object Test extends App {
  implicit class PimpedList[T](val list: List[T]) {
    def query(predicate: ReflectiveClosure[T, Boolean]): List[T] = {
      println(predicate.tree)
      list filter predicate
    }
  }

  List(1, 2, 3).query(x => x % 2 == 0)
}