//> using options -Ystop-after:parser
//
object foo {
  val n = 
    <a xmlns=""/>
  n.namespace == null
}
