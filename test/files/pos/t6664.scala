final case class A(i: Int, s: String) {
    protected def copy(s2: String): A = A(i, s2)
    protected def copy(i2: Int): A = A(i2, s)
}
