//> using options -Xlint:deprecation -Werror
object Test extends p1.DeprecatedClass {
  def useC = p1.DeprecatedClass.foo
  def useM = p1.DeprecatedMethod.foo
}
