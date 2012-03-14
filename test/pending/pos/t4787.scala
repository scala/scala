trait MatrixImpl[@specialized A, @specialized B] {
  def mapTo[ A2,  B2, That <: MatrixImpl[A2, B2]](that: That)(f: A => A2) {
  }
}
