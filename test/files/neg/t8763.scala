
import collection.mutable

object Foo {
  def bar(): Unit = {
    val names_times = mutable.Map[String, mutable.Set[Long]]()
    val line = ""
    val Array(fields) = line.split("\t")
    names_times(fields(0)) += fields(1).toLong
  }
}
