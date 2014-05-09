trait Trait40_1 {
  val value37_2 = ()
  def run = { value37_2 }
}

trait T1 extends Trait40_1 {
  override val value37_2 = ()
}

object Test {
  def main(args: Array[String]) {
    println((new T1 {}).run)
  }
}
