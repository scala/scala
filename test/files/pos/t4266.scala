object Test {

  trait Tensor2Like[
    @specialized(Int) A1,
    +D1 <: DomainLike[A1],
    +D <: Product2DomainLike[D1]
  ] {
    def domain: D;

    def checkKey(k1: A1) {
      domain._1.contains(k1)
    }
  }

  trait DomainLike[A] {
    def contains(key: A): Boolean;
  }

  // trait DomainLike[@specialized(Int) A] {
  //   def contains(key: A): Boolean;
  // }

  trait Product2DomainLike[+D1] {
    def _1: D1;
  }
}

