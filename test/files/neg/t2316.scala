object test {
  case class T1(val source: String)


  object T1 {
    implicit def T1FromT2(implicit t2: T2): T1 = T1("implicit def T1FromT2")
    implicit def T1FromT3(implicit t3: T3): T1 = T1("implicit def T1FromT3")
  }

  trait T2 {
  }

  object T2 {
    implicit val t2: T2 = new T2 {}
  }

  trait T3

  def requireT1(implicit t1: T1) = t1

  {
      val t1 = requireT1
      assert(t1.source == "implicit def T1FromT2")
  }

  {
      implicit def t3: T3 = new T3 {}
      val t1 = requireT1
      assert(t1.source == "implicit def T1FromT2")

      // Expected a compile error here, but because T1.T1FromT2(T2.t2) was cached as a non-local implicit
      // expression for type T1, this is not checked!
      //
      // (fragment of implicit-cache-error2.scala):26: error: ambiguous implicit values:
      // both method T1FromT3 in object T1 of type (implicit t3: this.T3)this.T1
      // and method T1FromT2 in object T1 of type (implicit t2: this.T2)this.T1
      // match expected type this.T1
      //    val t1 = requireT1
      //              ^
      // one error found

  }
}