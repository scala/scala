datatype sample code.

```scala:datatypeSample.scala
object datatypeSample {

  datatype Week = Mon | Tue | Wed | Thi | Fri | Sat | Sun

  val day: Week = Wed()
  val result = day match {
    case Mon() => "Monday"
    case Tue() => "Tuesday"
    case Wed() => "Wednesday"
    case Thi() => "Thirsday"
    case Fri() => "Friday"
    case Sat() => "Saturday"
    case Sun() => "Sunday"
  }

  datatype Week2 {
    def isWorkday: Boolean = true
    def isHolyday: Boolean = ! isWorkday
  }
  = Mon2 | Tue2 | Wed2 | Thi2 | Fri2 | Sat2 {
      override def isWorkday = false
    }
  | Sun2 {
      override def isWorkday = false
    }

  datatype Fig =
    Rect(val x: Double, val y: Double){
      def area(): Double = x * y
    }
  | Circle(val r: Double){
      val pi = 3.14
      def area(): Double = r * r * pi
  }

  def main(args: Array[String]): Unit = {
    println("result = " + result)

    val day2: Week2 = Sun2()
    println("Sun2.isWorkday = " + day2.isWorkday)
    println("Sun2.isHolyday = " + day2.isHolyday)

    val rect = Rect(3.0, 5.0)
    val area1 = rect.area()
    val circle = Circle(10.0)
    val area2 = circle.area()
    println("rect area = " + area1)
    println("circle area = " + area2)
  }
}
```