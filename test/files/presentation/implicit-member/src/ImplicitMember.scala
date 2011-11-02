object Implicit {

  final class AppliedImplicit[A](val x: A)
  
  implicit def AppliedImplicit[A](x: A): AppliedImplicit[A] = new AppliedImplicit(x)
  
  this./*!*/x
}