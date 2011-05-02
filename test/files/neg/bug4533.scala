package demo

import scala.collection._

class CrashDemo {
  def statusByAlarms(alarms: GenTraversableOnce[FooAlarm]) = println("hello")
}
class FooAlarm { }
