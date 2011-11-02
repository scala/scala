object NonEmptyCons {
  def unapply[H, T](c: (H, T)): Option[(H, T)] = Some(c)
}


object Main {

  type BT[+H, +T <: Tuple2[Tuple2[H, T], Tuple2[H, T]]] = Tuple2[H, T]
  
  // type T = Tuple2[String,String]
  type BinaryTree[+E] = BT[E, T forSome { type T <: Tuple2[BT[E, T], BT[E, T]] }]

  def foo[E](tree: BinaryTree[E]): Unit = tree match {
    case NonEmptyCons(_, tail) => {
      tail match {
        case NonEmptyCons(_, _) => {
        }
      }
    }
  }
}