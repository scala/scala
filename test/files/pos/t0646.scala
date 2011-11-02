object xfor {

  import scala.xml.NodeSeq

    val books = 
    <bks>
      <title>Blabla</title>
      <title>Blubabla</title>
      <title>Baaaaaaalabla</title>
    </bks>;

  new NodeSeq { val theSeq = books.child }  match {
    case t @ Seq(<title>Blabla</title>) => t
  }

  //val n: NodeSeq = new NodeSeq { val theSeq = books.child } 
  //n match {
  //  case t @ <title>Blabla</title> => t
  //}

}
