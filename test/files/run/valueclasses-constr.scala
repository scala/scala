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

object Test extends App {

  val y: TOD = new TOD(1000)
  val x: TOD = TOD(1000)
  println(x.hours)
  println(x)
}

