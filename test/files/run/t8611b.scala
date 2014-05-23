sealed trait KrafsDescription

abstract class NotWorkingEnum extends Enumeration {

  type ExtendedValue = Value with KrafsDescription

  def Enum(inDescription: String): ExtendedValue = {
    new Val(nextId) with KrafsDescription {
    }
  }
}

abstract class WorkingEnum extends Enumeration {

  type ExtendedValue = Value

  def Enum(inDescription: String): ExtendedValue = {
    new Val(nextId) {
    }
  }
}

object NotWorkingTab extends NotWorkingEnum {
  val a = Enum("A")
  val b = Enum("B")
}

object WorkingTab extends WorkingEnum {
  val a = Enum("A")
  val b = Enum("B")
}

object Test extends App {
  testGris()
  testWorking()

  def testGris() {
    val pipp = NotWorkingTab.b
    pipp match {
      case NotWorkingTab.a => ???
      case NotWorkingTab.b =>
      case _ => ???
    }
  }

  def testWorking() {
    val stuff = WorkingTab.a
    stuff match {
      case WorkingTab.a =>
      case WorkingTab.b => ???
      case _ => ???
    }
  }
}
