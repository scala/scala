object TailrecAfterTryCatch {

  @annotation.tailrec
  final def good1(): Unit = {
    1 match {
      case 2 => {
        try {
        //  return
        } catch {
          case e: ClassNotFoundException =>
        }
        good1()
      }
    }
  }

  @annotation.tailrec
  final def good2(): Unit = {
    //1 match {
    //  case 2 => {
        try {
          return
        } catch {
          case e: ClassNotFoundException =>
        }
        good2()
    //  }
    //}
  }

  @annotation.tailrec
  final def good3(): Unit = {
    val 1 = 2
    try {
      return
    } catch {
      case e: ClassNotFoundException =>
    }
    good3()
  }

  @annotation.tailrec
  final def bad(): Unit = {
    1 match {
      case 2 => {
        try {
          return
        } catch {
          case e: ClassNotFoundException =>
        }
        bad()
      }
    }
  }

}
