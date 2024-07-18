//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml =
    <root>
      &amp; &quot; &#x27; &lt; &gt;
    </root>
}
