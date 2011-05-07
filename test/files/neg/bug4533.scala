package demo

import scala.collection._

class CrashDemo {
  def statusByAlarms(alarms: GenSetLike[FooAlarm]) = println("hello")
}
class FooAlarm { }
