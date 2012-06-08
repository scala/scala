package compilerbug

trait SadTrait {
  def buggyMethod[T](argWithDefault1: Int = 0)(argWithDefault2: String = "default") {
    for (i <- 0 to 1) {
      val x = argWithDefault1
      val y = argWithDefault2
    }
  }
}

object SadObject extends SadTrait
