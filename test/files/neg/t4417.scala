



class Pixel[@specialized T] protected (var v: T)


object Pixel {
  type Pixel1d = Pixel[Double]

  def apply(v: Double): Pixel1d = new Pixel1d(v)
}





