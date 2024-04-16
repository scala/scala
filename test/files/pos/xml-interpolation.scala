//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml = 
    <root>
      {bar}
      { bar }
      {{ bar }}
      {{ 3 }}
      {{3}}
    </root>
}
