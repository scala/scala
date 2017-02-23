
import reflect.SourceContext

class Matrix {
  def numCols(implicit sc: SourceContext): Int = {
    println("enter numCols")
    println("method: "+sc.methodName)
    println("receiver: "+sc.receiver)
    0
  }
}

object Test {

  def relative(name: String) = {
    val lastSlash = name.lastIndexOf('/')
    if (lastSlash == -1)
      name.substring(name.lastIndexOf('\\') + 1)
    else
      name.substring(lastSlash + 1)
  }

  def printInfo(m: SourceContext) {
    println("line: "+m.line)
    println("binding: "+m.bindings(0)._1)
    println("method name: "+m.methodName)
    println("contexts: "+m.allContexts)
    println("file name: "+relative(m.fileName))
  }

  def printShortInfo(sc: SourceContext) {
    println("method: "+sc.methodName)
    println("line: "+sc.line)
    if (!sc.parent.isEmpty)
      println("parent.line: "+sc.parent.get.line)
  }

  def inspect[T](x: T)(implicit m: SourceContext): Int = {
    def withManifest()(implicit mm: SourceContext) {
      printInfo(mm)
    }
    printInfo(m)
    withManifest()
    0
  }

  def testUpdate2()(implicit sc: SourceContext) {
    println("invocation:")
    printShortInfo(sc)
  }

  def testUpdate()(implicit sc: SourceContext) {
    println("invocation:")
    printShortInfo(sc)
    // this should pass sc.update(<new SourceContext>)
    testUpdate2()
  }

  def sum(start: Int, end: Int)(block: Int => Int)(implicit sc: SourceContext) {
    println("enter sum")
    println("method: "+sc.methodName)
  }

  def main(args: Array[String]) {
    val l = List(1, 2, 3)
    val x = inspect(l)
    val y = {
      val z = 4*7
      inspect(l)
    }

    // test SourceContext.update
    testUpdate()

    // test method without parameter list
    val m = new Matrix
    val n = m.numCols

    val sigma = sum(0, n) { (i: Int) => i }
  }

}
