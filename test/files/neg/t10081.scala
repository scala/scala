trait A[_]
trait B[X] extends A[B[X @unchecked]] { this.x }
