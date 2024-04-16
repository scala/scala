//> using options -Ystop-after:parser
//
object foo {
  val bar = "baz"
  val xml =
    <root lang="en" type='xml'>
      <child attr="&quot;" foo={bar}/>
      <child ns:attr="" foo={bar}/>
    </root>
}
