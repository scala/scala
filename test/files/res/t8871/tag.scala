class Tag {
  @inline def apply[@specialized A, T](a: A): A = a
}
