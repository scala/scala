// scalac: -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml = 
    <root>
      {bar}
      { bar }
      {{ bar }}
    </root>
}
