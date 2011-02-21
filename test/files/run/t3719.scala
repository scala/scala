object Days extends Enumeration {
  type Day = DayValue
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = new DayValue // DayValue

  protected class DayValue extends Val {
    def isWeekday: Boolean =
      this match {
        case Sun => false
        case Sat => false
        case _ => true
      }
  }
}

object Test extends App {
  def dayElementsShouldBeNamed(): List[String] =
    Days.values.toList.sorted.map(x => x.toString)

  def nameOfMon(): String =  {
    import Days._
    val d: Day = Mon
    d.toString
  }

  def nameOfTue(): String =  {
    import Days._
    val d: Day = Tue
    d.toString
  }

  println(dayElementsShouldBeNamed())
  println(nameOfMon())
  println(nameOfTue())
  println(nameOfMon())
}
