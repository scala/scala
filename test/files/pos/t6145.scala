object Test {
  // the existential causes a cast and the cast makes searchClass not be in tail position
  // can we get rid of the useless cast?
  @annotation.tailrec
  final def searchClass: Class[_] = {
    "packageName" match {
      case _ =>
        searchClass
    }
  }
}