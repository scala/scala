object Test extends App {
  import collection.mutable.ListBuffer
  
  def newLB = ListBuffer('a, 'b, 'c, 'd, 'e)

  val lb0 = newLB

  try {
    lb0.insert(-1, 'x)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  val lb1 = newLB

  try {
    lb1.insertAll(-2, Array('x, 'y, 'z))
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  val lb2 = newLB

  try {
    lb2.update(-3, 'u)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }
}