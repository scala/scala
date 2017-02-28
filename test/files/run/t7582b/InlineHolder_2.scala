package p1 {
  object InlineHolder {
    @inline def inlinable = p1.PackageProtectedJava_1.protectedMethod() + 1
  }
}

object O {
  @noinline
  def x = p1.InlineHolder.inlinable
}

object Test {
  def main(args: Array[String]) {
    println(O.x)
  }
}
