datatype example code.


```scala:datatypeSample.scala
object datatypeSample {

  datatype Week = Mon | Tue | Wed | Thi | Fri | Sat | Sun

  val day: Week = Wed()
  val result = day match {
    case Mon() => "月曜日"
    case Tue() => "火曜日"
    case Wed() => "水曜日"
    case Thi() => "木曜日"
    case Fri() => "金曜日"
    case Sat() => "土曜日"
    case Sun() => "日曜日"
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