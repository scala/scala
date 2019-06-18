object Test extends tools.partest.ReplTest {
  override def code =
    """
    | // should say "unbound wildcard type" not "not found: type _$1"
    | type T = _
    | class C extends _
    | trait F[x <: _]
    """.stripMargin
}