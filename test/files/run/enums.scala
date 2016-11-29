@enum class Empty {}
@enum class ReallyEmpty

@enum class Week {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

@enum class ConstructorWeek(val isWeekend: Boolean) {
  Monday(false)
  Tuesday(false)
  Wednesday(false)
  Thursday(false)
  Friday(false)
  Saturday(true)
  Sunday(true)

  def isWeekendMissing = this match {
    case Saturday
       | ConstructorWeek.Sunday   => true
  }
}

@enum class MethodWeek {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday   { val isGoodDay = true }
  Saturday { def isGoodDay = true }
  Sunday   { def isGoodDay = true }

  def isGoodDay: Boolean = false

  def isWeekend = this match {
    case Saturday
       | MethodWeek.Sunday   => true
    case _                   => false
  }
  def isWeekendMissing = this match {
    case Saturday
       | MethodWeek.Sunday   => true
  }
}

@enum class ArgNamedWeek(val isWeekend: Boolean) {
  Monday(false)
  Tuesday(false)
  Wednesday(false)
  Thursday(false)
  Friday(false)            { val isGoodDay = true }
  Saturday(true)           { def isGoodDay = true }
  Sunday(isWeekend = true) { def isGoodDay = true }

  def isGoodDay: Boolean = false
  def isWeekendMissing = this match {
    case Saturday
       | ArgNamedWeek.Sunday   => true
  }
}

@enum case class CaseWeek(isWeekend: Boolean) {
  Monday(false)
  Tuesday(false)
  Wednesday(false)
  Thursday(false)
  Friday(false)            { val isGoodDay = true }
  Saturday(true)           { def isGoodDay = true }
  Sunday(isWeekend = true) { def isGoodDay = true }

  def isGoodDay: Boolean = false
  def isWeekendCase = this match {
    case Saturday //(true)
       | CaseWeek.Sunday   => true
  }
  def isWeekendMissing = this match {
    case Saturday
       | CaseWeek.Sunday   => true
  }
}

/*
@enum class ArgDefaultWeek(isWeekend: Boolean = false) {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday(true)
  Sunday(true)
}

object Nested {
  @enum class Week {
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
  }
}

class Nested {
  @enum class Week {
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
  }
}
*/

/*
@enum sealed abstract class EmptyADT

@enum sealed abstract class ADT
object One extends ADT

@enum sealed abstract class CaseADT
case object One

@enum sealed abstract class NestedADT
object NestedADT {
  object One
}

@enum sealed abstract class NestedCaseADT
object NestedCaseADT {
  case object One
}
*/

object Test extends App {

  printEnumInfo(classOf[Empty])
  println(Empty.values.toList)
  println()

  printEnumInfo(classOf[ReallyEmpty])
  println(ReallyEmpty.values.toList)
  println()

  printEnumInfo(classOf[Week])
  println(Week.Monday.getClass.isEnum)
  println(Week.values.toList)
  println(Week.valueOf("Friday") == Week.Friday)
  println(Week.Saturday.compareTo(Week.Sunday) < 0)
  println(isWeekendMissing(Week.Sunday))
  println(!isWeekendComplete(Week.Thursday))
  println()
  // warning: match may not be exhaustive.
  // It would fail on the following inputs: Friday, Monday, Tuesday, Wednesday, Week()
  def isWeekendMissing(week: Week) = week match {
    case Week.Saturday
       | Week.Sunday   => true
  }
  def isWeekendComplete(week: Week) = week match {
    case Week.Saturday
       | Week.Sunday   => true
    case _             => false
  }

  printEnumInfo(classOf[ConstructorWeek])
  println(ConstructorWeek.Monday.getClass.isEnum)
  println(ConstructorWeek.values.toList)
  println(ConstructorWeek.valueOf("Friday") == ConstructorWeek.Friday)
  println(ConstructorWeek.Saturday.compareTo(ConstructorWeek.Sunday) < 0)
  println(ConstructorWeek.values.toList.partition(_.isWeekend))
  println()

  printEnumInfo(classOf[MethodWeek])
  println(MethodWeek.Monday.getClass.isEnum)
  println(MethodWeek.values.toList)
  println(MethodWeek.valueOf("Friday") == MethodWeek.Friday)
  println(MethodWeek.Saturday.compareTo(MethodWeek.Sunday) < 0)
  println(MethodWeek.values.toList.partition(_.isWeekend))
  println(MethodWeek.values.toList.partition(_.isGoodDay))
  println()

  printEnumInfo(classOf[ArgNamedWeek])
  println(ArgNamedWeek.Monday.getClass.isEnum)
  println(ArgNamedWeek.values.toList)
  println(ArgNamedWeek.valueOf("Friday") == ArgNamedWeek.Friday)
  println(ArgNamedWeek.Saturday.compareTo(ArgNamedWeek.Sunday) < 0)
  println(ArgNamedWeek.values.toList.partition(_.isWeekend))
  println(ArgNamedWeek.values.toList.partition(_.isGoodDay))
  println()

  printEnumInfo(classOf[CaseWeek])
  println(CaseWeek.Monday.getClass.isEnum)
  println(CaseWeek.values.toList)
  println(CaseWeek.valueOf("Friday") == CaseWeek.Friday)
  println(CaseWeek.Saturday.compareTo(CaseWeek.Sunday) < 0)
  println(CaseWeek.values.toList.partition(_.isWeekend))
  println(CaseWeek.values.toList.partition(_.isGoodDay))

  def modifiersToString(flags: Int): String = {
    import java.lang.reflect.Modifier
    var buf = new StringBuilder
    if (Modifier.isAbstract (flags)) buf ++= "ABSTRACT "
    if (Modifier.isFinal    (flags)) buf ++= "FINAL "
    if (Modifier.isInterface(flags)) buf ++= "INTERFACE "
    if (Modifier.isPrivate  (flags)) buf ++= "PRIVATE "
    if (Modifier.isProtected(flags)) buf ++= "PROTECTED "
    if (Modifier.isPublic   (flags)) buf ++= "PUBLIC "
    if (Modifier.isStatic   (flags)) buf ++= "STATIC "
    buf = buf.init
    buf.toString()
  }

  def printEnumInfo(clazz: Class[_ <: Enum[_]]): Unit = {
    println(s"==== enum ${clazz.getSimpleName} ====")
    println(clazz.isEnum)
    println(clazz.getModifiers)
    println(modifiersToString(clazz.getModifiers))
    println(clazz.getSuperclass)
    println(clazz.getInterfaces.toList)
    println(clazz.getDeclaredConstructors.toList)
    println(clazz.getDeclaredMethods.toList.sortBy(_.getName))
    println(clazz.getDeclaredFields.toList.sortBy(_.getName))
  }
}
