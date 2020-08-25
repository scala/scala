// scalac: -Xfatal-warnings -deprecation
object Test {
  def useC = p1.DeprecatedClass.foo
  def useM = p1.DeprecatedMethod.foo
}
