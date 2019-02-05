class t8583 {

  case class A( value: Double ) {
    def *( o: A ) = A( value * o.value )
  }

  implicit def doubleToA( d: Double ) = A( d )
  implicit def listToA( in: List[A] ): A = in.head

  val result: A = List( A( 1 ) ) map { 2.0 * _ } //this line causes the compiler to crash
}
