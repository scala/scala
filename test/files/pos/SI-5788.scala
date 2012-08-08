trait Test {
  trait B[T]
  private final def grow[T](): B[T] = grow[T]()
}
