object RClose {
  type ReflectCloseable = { def close(): Unit }
  def withReflectCloseable[T <: ReflectCloseable, R](s: T)(action: T => R): R =
    try {
      action(s)
    } finally {
      s.close()
    }
}
