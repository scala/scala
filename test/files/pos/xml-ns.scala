//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml = 
    <foo:root>
      <bar:child baz:foo={bar}>{bar}</bar:child>
    </foo:root>
}
