trait Complex extends Product2[Double,Double] 

class ComplexRect(val _1:Double, _2:Double) extends Complex {
  override def toString = "ComplexRect("+_1+","+_2+")"
}

object Test {
  def main(args:Array[String]) = {
    new ComplexRect(1,1)._2
  }
}
