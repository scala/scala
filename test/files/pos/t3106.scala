class Sample[A] (val d0: ((A,A)) => A) {}

object Sample {
  implicit def apply[A] (x:A): Sample[A] = {
    new Sample(p => p._1)
  }
}