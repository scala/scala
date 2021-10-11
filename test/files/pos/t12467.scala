object PagedResponse {
  type Aux[Item0] = PagedResponse { type Item = Item0 }
}

trait PagedResponse {
  type Item
  sealed trait NextPage
  case class NoMorePages() extends NextPage
}

object Test {
  def foo[A](next: PagedResponse.Aux[A]#NextPage): Unit = next match {
    case _: PagedResponse.Aux[A]#NoMorePages => ???
  }
}
