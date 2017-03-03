object Test {
  def assertApply(expected: Boolean) = {
    val frames = Thread.currentThread.getStackTrace.takeWhile(_.getMethodName != "main")
    val usesObjectApply = frames.exists(_.getMethodName == "apply")
    assert(expected == usesObjectApply, frames.mkString("\n"))
  }
  def assertSpecialized() = assertApply(false)
  def assertUnspecialized() = assertApply(true)
  def main(args: Array[String]): Unit = {
    ((i: String)      => {assertUnspecialized(); i}).apply("")
    (()               => {assertSpecialized(); 0}).apply()
    ((i: Int)         => {assertSpecialized(); i}).apply(0)
    ((i: Int, j: Int) => {assertSpecialized(); i + j}).apply(0, 0)
  }
}
