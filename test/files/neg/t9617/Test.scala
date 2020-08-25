// scalac: -Xfatal-warnings -deprecation
// Joint-compilation copy of test/files/neg/t10752/Test_2.scala
object Test {
  def useC = p1.DeprecatedClass.foo
  def useM = p1.DeprecatedMethod.foo
}
