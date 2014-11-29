import collection.mutable.ListBuffer

object Test extends App {
  def newLB = ListBuffer('a, 'b, 'c, 'd, 'e)

  val lb0 = newLB
  println("Trying lb0 ...")
  try {
    lb0.remove(5, 0)
  } catch {
    // Should not be thrown--nothing is deleted so nothing to do
    case ex: IndexOutOfBoundsException => println(ex)
  }
  checkNotCorrupted(lb0)

  val lb1 = newLB
  println("Trying lb1 ...")
  try {
    lb1.remove(6, 6)
 } catch {
    // Not thrown in 2.11, is thrown in 2.12
    case ex: IndexOutOfBoundsException => println(ex)
  }
  checkNotCorrupted(lb1)

  val lb2 = newLB
  println("Trying lb2 ...")
  try {
    lb2.remove(99, 6)
  } catch {
    // Not thrown in 2.11, is thrown in 2.12
    case ex: IndexOutOfBoundsException => println(ex)
  }
  checkNotCorrupted(lb2)

  val lb3 = newLB
  println("Trying lb3 ...")
  try {
    lb3.remove(1, 9)
  } catch {
    // Not thrown in 2.11, is thrown in 2.12
    case ex: IndexOutOfBoundsException => println(ex)
  }
  checkNotCorrupted(lb3)

  val lb4 = newLB
  println("Trying lb4 ...")
  try {
    lb4.remove(-1, 1)
  } catch {
    // Not thrown in 2.11, is thrown in 2.12
    case ex: IndexOutOfBoundsException => println(ex)
  }
  checkNotCorrupted(lb4)

  val lb5 = newLB
  println("Trying lb5 ...")
  try {
    lb5.remove(1, -1)
  } catch {
    // Was thrown prior to 2.12 also
    case ex: IllegalArgumentException => println(ex)
  }
  checkNotCorrupted(lb5)

  // buffer should neither be changed nor corrupted after calling remove with invalid arguments
  def checkNotCorrupted(
    lb: ListBuffer[Symbol],
    expectedString: String = "ListBuffer('a, 'b, 'c, 'd, 'e)",
    expectedLength: Int = 5) = {
    println("Checking ...")
    val replStr = scala.runtime.ScalaRunTime.replStringOf(lb, 100)
    if (replStr == expectedString + "\n") println("String OK.")
    else println("!!! replStringOf FAILED: " + replStr)

    val len = lb.length
    if (len == expectedLength) println("Length OK.")
    else println("!!! length FAILED: " + len)
    println()
  }
}
