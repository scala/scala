//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml =
    <root>
      <![CDATA[]]>
      <![CDATA[{bar}]]>
    </root>
  val pcdata =
      <![CDATA[]]>
      <![CDATA[{bar}]]>
}
