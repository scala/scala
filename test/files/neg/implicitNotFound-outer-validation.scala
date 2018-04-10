import annotation.{implicitNotFound => inf}

package `all-of-these-work` {
  @inf("${O}")
  trait Outer[O] {
    @inf("${O} ${M}")
    trait Mid[M] {
      @inf("${O} ${M} ${I}")
      trait Inner[I]
    }

    object Meh extends Mid[Int] { meh =>
      @inf("${O} ${Q}")
      trait Hrem[Q] extends meh.Inner[Q]
    }
  }
}

package `none-of-these-do` {
  /* these don't */
  @inf("${M} ${Q}")
  trait Outer[O] {
    @inf("${I} ${Q}")
    trait Mid[M] {
      @inf("${Q} ${Thwop}")
      trait Inner[I]
    }

    object Meh extends Mid[Int] { meh =>
      @inf("${M} ${I}")
      trait Hrem[Q] extends meh.Inner[Q]
    }

  }

}
