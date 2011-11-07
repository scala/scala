object Test extends App
{
  val xs = List(
    <a></a>,
    <a/>,
    <a>{ xml.NodeSeq.Empty }</a>,
    <a>{""}</a>,
    <a>{ if (true) "" else "I like turtles" }</a>
  )
  
  for (x1 <- xs; x2 <- xs) assert (x1 xml_== x2)
}
