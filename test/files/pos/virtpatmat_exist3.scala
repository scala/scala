class ReferenceQueue[T] {
  def wrapper(jref: ReferenceQueue[_]): ReferenceQueue[T] =
    jref match {
      case null => null
    }

  // def wrapper(jref: ReferenceQueue[_]): ReferenceQueue[T] = OptionMatching.runOrElse(jref)(((x1: ReferenceQueue[_]) =>
  //   (OptionMatching.guard(null.==(x1), x1.asInstanceOf[ReferenceQueue[_]]).flatMap(((x2: ReferenceQueue[_]) =>
  //     OptionMatching.one(null))): Option[ReferenceQueue[T]]).orElse(
  //   (OptionMatching.zero: Option[ReferenceQueue[T]])))
  // )
}