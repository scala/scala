//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml = 
    <root>
      <!---->
      <!-- {bar} --->
    </root>
}
