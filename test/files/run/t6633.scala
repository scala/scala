object Test extends App {
  import collection.mutable.ListBuffer

  def newLB = ListBuffer('a, 'b, 'c, 'd, 'e)

  val lb0 = newLB

  try {
    lb0.insert(9, 'x)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  val lb1 = newLB

  try {
    lb1.insert(9, 'x)
  } catch {
    case ex: IndexOutOfBoundsException =>
  }

  val replStr = scala.runtime.ScalaRunTime.replStringOf(lb1, 100)
  if (replStr == "ListBuffer('a, 'b, 'c, 'd, 'e)\n")
    println("replStringOf OK")
  else
    println("replStringOf FAILED: " + replStr)

  val len = lb1.length
  if (len == 5)
    println("length OK")
  else
    println("length FAILED: " + len)
}