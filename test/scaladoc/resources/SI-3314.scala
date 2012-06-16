package scala.test.scaladoc {

  package test1 {
    class Enum {
      abstract class Value
      class Val extends Value
      def Value(): Value = new Val
    }

    object Constants extends Enum {
      def a = Value
    }
  }

  package test2 {
    trait WeekDayTrait extends Enumeration {
      type WeekDay = Value
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }

    class WeekDayClass extends Enumeration {
      type WeekDay = Value
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }

    object WeekDayObject extends Enumeration {
      type WeekDay = Value
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }

    object UserObject {
      def isWorkingDay1(d: scala.test.scaladoc.test2.WeekDayClass#Value) = false
      def isWorkingDay2(d: scala.test.scaladoc.test2.WeekDayClass#WeekDay) = false
      def isWorkingDay3(d: scala.test.scaladoc.test2.WeekDayTrait#Value) = false
      def isWorkingDay4(d: scala.test.scaladoc.test2.WeekDayTrait#WeekDay) = false
      def isWorkingDay5(d: scala.test.scaladoc.test2.WeekDayObject.Value) = false
      def isWorkingDay6(d: scala.test.scaladoc.test2.WeekDayObject.WeekDay) = false
      import WeekDayObject._
      def isWorkingDay7(d: Value) = ! (d == Sat || d == Sun)
      def isWorkingDay8(d: WeekDay) = ! (d == Sat || d == Sun)
      def isWorkingDay9(d: WeekDayObject.Value) = ! (d == Sat || d == Sun)
    }

    class UserClass {
      def isWorkingDay1(d: scala.test.scaladoc.test2.WeekDayClass#Value) = false
      def isWorkingDay2(d: scala.test.scaladoc.test2.WeekDayClass#WeekDay) = false
      def isWorkingDay3(d: scala.test.scaladoc.test2.WeekDayTrait#Value) = false
      def isWorkingDay4(d: scala.test.scaladoc.test2.WeekDayTrait#WeekDay) = false
      def isWorkingDay5(d: scala.test.scaladoc.test2.WeekDayObject.Value) = false
      def isWorkingDay6(d: scala.test.scaladoc.test2.WeekDayObject.WeekDay) = false
      import WeekDayObject._
      def isWorkingDay7(d: Value) = ! (d == Sat || d == Sun)
      def isWorkingDay8(d: WeekDay) = ! (d == Sat || d == Sun)
      def isWorkingDay9(d: WeekDayObject.Value) = ! (d == Sat || d == Sun)
    }

    trait UserTrait {
      def isWorkingDay1(d: scala.test.scaladoc.test2.WeekDayClass#Value) = false
      def isWorkingDay2(d: scala.test.scaladoc.test2.WeekDayClass#WeekDay) = false
      def isWorkingDay3(d: scala.test.scaladoc.test2.WeekDayTrait#Value) = false
      def isWorkingDay4(d: scala.test.scaladoc.test2.WeekDayTrait#WeekDay) = false
      def isWorkingDay5(d: scala.test.scaladoc.test2.WeekDayObject.Value) = false
      def isWorkingDay6(d: scala.test.scaladoc.test2.WeekDayObject.WeekDay) = false
      import WeekDayObject._
      def isWorkingDay7(d: Value) = ! (d == Sat || d == Sun)
      def isWorkingDay8(d: WeekDay) = ! (d == Sat || d == Sun)
      def isWorkingDay9(d: WeekDayObject.Value) = ! (d == Sat || d == Sun)
    }
  }
}
