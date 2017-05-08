package scala.test.scaladoc {

  // testing inherited <documented> templates (Enum.Value is included in the source, thus is documented in scaladoc)
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

  // testing inherited <not documented> templates (scala.Enumeration.Value is taken from the library, thus is not
  // documented in the scaladoc pages -- but should be inherited to make things clear!)
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

  // testing type lambdas and useless prefixes (should be referenced as T instead of foo.T in the first example)
  package test3 {
    import language.higherKinds
    object `package` {
      trait T
      trait A
      trait X
      def foo[T](x: T) = 7
      def bar[A](x: ({type Lambda[X] <: Either[A, X]})#Lambda[String]) = 5
    }
  }
}
