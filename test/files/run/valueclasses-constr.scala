package test1 {
  object TOD {
   final val SecondsPerDay = 86400

   def apply(seconds: Int) = {
     val n = seconds % SecondsPerDay
     new TOD(if (n >= 0) n else n + SecondsPerDay)
   }
  }

  final class TOD (val secondsOfDay: Int) extends AnyVal {
   def hours = secondsOfDay / 3600
   def minutes = (secondsOfDay / 60) % 60
   def seconds = secondsOfDay % 60

   override def toString = "%02d:%02d:%02d".format(hours, minutes, seconds)
  }
}
package test2 {
  object TOD {
   final val SecondsPerDay = 86400

   def apply(seconds: Int) = {
     val n = seconds % SecondsPerDay
     new TOD(if (n >= 0) n else n + SecondsPerDay)
   }
  }

  final class TOD private[test2] (val secondsOfDay: Int) extends AnyVal {
   def hours = secondsOfDay / 3600
   def minutes = (secondsOfDay / 60) % 60
   def seconds = secondsOfDay % 60

   override def toString = "%02d:%02d:%02d".format(hours, minutes, seconds)
  }

  object Client {
    def newTOD(x: Int) = new TOD(x)
  }
}

package test3 {
  object TOD {
   final val SecondsPerDay = 86400

   def apply(seconds: Int) = {
     val n = seconds % SecondsPerDay
     new TOD(if (n >= 0) n else n + SecondsPerDay)
   }
  }

  final class TOD private (val secondsOfDay: Int) extends AnyVal {
   def hours = secondsOfDay / 3600
   def minutes = (secondsOfDay / 60) % 60
   def seconds = secondsOfDay % 60

   override def toString = "%02d:%02d:%02d".format(hours, minutes, seconds)
  }
}

object Test extends App {

  val y1: test1.TOD = new test1.TOD(1000)
  val y2: test2.TOD = test2.Client.newTOD(1000)
  val x1: test1.TOD = test1.TOD(1000)
  val x2: test2.TOD = test2.TOD(1000)
  val x3: test3.TOD = test3.TOD(1000)
  println(y1.minutes)
  println(y1)
  println(y2.minutes)
  println(y2)
  println(x1.minutes)
  println(x1)
  println(x2.minutes)
  println(x2)
  println(x3.minutes)
  println(x3)
}

