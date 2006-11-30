trait Complex extends Product2[double,double]

class ComplexRect(val _1:double, _2:double) extends Complex {
  override def toString = "ComplexRect("+_1+","+_2+")"
}

object Test {
  def main(args:Array[String]) = {
    new ComplexRect(1,1)._2
  }
}
