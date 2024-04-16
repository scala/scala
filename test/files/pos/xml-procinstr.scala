//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml =
    <?xml-stylesheet href="style.xslt" type="text/xsl"?>
    <root>
      <?foo {bar}?>
    </root>
}
