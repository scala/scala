object App
{
  def exponentiate(base : Double, exponent : Double) : Double =
      (base, exponent) match
      {
        case (0, 0) => 1.0
        case (b, 0) => 1.0
        case (b, 1) => b
        case (b, e) => b * exponentiate(b, e - 1)
      }



  def main(args : Array[String]) =
    System.out.println(exponentiate(2, 2))

}

