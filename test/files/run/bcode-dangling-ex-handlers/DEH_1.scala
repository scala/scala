
class DEH_1 {

  def m() {
    try {
      if (false) { println("if-branch") }
    } catch {
      case _: Throwable => println("catch-clause")
    }
  }

}
