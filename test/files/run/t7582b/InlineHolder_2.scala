//> using options -opt:inline:** -Wopt
package p1 {
  object InlineHolder {
    @inline def inlinable = (p1.PackageProtectedJava_1.protectedMethod(): @noinline) + 1
  }
}

object O {
  @noinline
  def x = p1.InlineHolder.inlinable
}

object Test {
  def main(args: Array[String]): Unit = {
    println(O.x)
  }
}
