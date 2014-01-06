object TailrecAfterTryCatch {

  @annotation.tailrec
  final def good1() {
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
  final def good2() {
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
  final def good3() {
    val 1 = 2
    try {
      return
    } catch {
      case e: ClassNotFoundException =>
    }
    good3()
  }

  @annotation.tailrec
  final def bad() {
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