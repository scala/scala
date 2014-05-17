object RefinementTest {
  sealed trait Grandparent[A]
  sealed trait Parent[A] extends Grandparent[A]
  case class Child[C](c: (C, C)) extends Parent[(C, C)]

  def extractParent[A](parent: Parent[A]): A = parent match {
    case child: Child[c] => child.c
  }

  // Fails
  def extractGrandparent1[A](grandparent: Grandparent[A]): A = grandparent match {
    case parent: Parent[a] =>
      parent match {
        //     Error: type mismatch;
        //     found   : (c, c)
        //     required: a
        case child: Child[c] => child.c: a
      }
  }

  // Works
  def extractGrandparent2[A](grandparent: Grandparent[A]): A = grandparent match {
    case parent: Parent[a] => extractParent(parent)
  }
}
