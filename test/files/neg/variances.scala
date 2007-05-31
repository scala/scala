trait Vector[+A] {
  def append(x: Vector[A]): Vector[A]
  private[this] def append3(x: Vector[A]): Vector[A] = append(x)
}

