//> using options -Werror -Xlint:deprecation

// Joint-compilation copy of test/files/neg/t10752/Test_2.scala
object Test extends p1.DeprecatedClass {
  def useC = p1.DeprecatedClass.foo
  def useM = p1.DeprecatedMethod.foo
}
