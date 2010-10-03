class Mxml {
  private def processChildren( children:Seq[Any] ):List[Mxml] = {
    children.toList.flatMap ( e => {
      e match {
        case s:scala.collection.Traversable[_] => s case a => List(a)
      }
    })
  }
}
