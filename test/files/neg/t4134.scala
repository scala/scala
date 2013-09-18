


trait T1 {
  def f: String
}

trait T2 extends T1 {
  abstract override def f: String = "goo"
  def something = super.f  // So the "abstract override" is needed
}

trait Q1 {
  def f: String = "bippy"
}

//trait T3 extends Q1 with T2 {
trait T3 extends T2 with Q1 {
  abstract override def f: String = super[Q1].f + " " + super[T2].f + " hoo"
}

class Konkret extends T3

object Test {
  def main(args: Array[String]): Unit = {
    val k = new Konkret
    println(k.f)
    println(k.something)
  }
}
