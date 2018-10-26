trait B[+T] {
  def f: B.this.type
}

trait W
trait WB extends B[W]  {
  final def f = g
  def g: this.type
}
class WBImpl extends WB {
  def g = this
}

trait P extends W
trait PB extends WB with B[P]
class PBImpl extends WBImpl with PB {
  override def g = this
  override def toString = "PBImpl"
}

object Test {
  def main(args: Array[String]): Unit = {
    val p = new PBImpl
    println(p.f)
  }
}
