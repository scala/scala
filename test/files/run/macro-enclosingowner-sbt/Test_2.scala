object Test extends App {
  val a1 = Macros.foo
  val a2 = Predef.identity(Predef.identity(Macros.foo))
  val a3: Int = Macros.foo
  val a4: Int = Predef.identity(Predef.identity(Macros.foo))

  var b1 = Macros.foo
  var b2 = Predef.identity(Predef.identity(Macros.foo))
  var b3: Int = Macros.foo
  var b4: Int = Predef.identity(Predef.identity(Macros.foo))

  def c1 = Macros.foo
  def c2 = Predef.identity(Predef.identity(Macros.foo))
  def c3: Int = Macros.foo
  def c4: Int = Predef.identity(Predef.identity(Macros.foo))
  c1; c2; c3; c4;

  lazy val d1 = Macros.foo
  lazy val d2 = Predef.identity(Predef.identity(Macros.foo))
  lazy val d3: Int = Macros.foo
  lazy val d4: Int = Predef.identity(Predef.identity(Macros.foo))
  d1; d2; d3; d4
}