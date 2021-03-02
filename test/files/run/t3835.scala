object Test extends App {
  def a = (1, 2, 3) match { case (r, θ, φ) => r + θ + φ }
  println(a)
  def b = (1 match { case é => é })
  println(b)
}
