// scalac: -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml =
    <root>
      <![CDATA[]]>
      <![CDATA[{bar}]]>
    </root>
}
