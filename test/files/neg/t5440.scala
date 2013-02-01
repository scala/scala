object Test {
  def merge(list1: List[Long], list2: List[Long]): Boolean =
    (list1, list2) match {
      case (hd1::_, hd2::_) => true
      case (Nil, Nil) => true
    }
}