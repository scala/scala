trait BugBase [A, E] {
  val key: A
  var next: E = _
}

final class Bug[A, B](val key: A) extends BugBase[A, Bug[A, B]] {
  def foo = next
}

object Test {
  def f(clazz: Class[_]) =
    clazz.getDeclaredMethods.toList.map(_.toGenericString).sorted foreach println
  
  def main(args: Array[String]): Unit = {
    f(classOf[Bug[_, _]])
    f(classOf[BugBase[_, _]])
  }
}
