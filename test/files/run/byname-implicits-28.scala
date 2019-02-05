object Loop0 {
  trait Link0 {
    def next0: Link0
  }
  object Link0 {
    implicit def mkLink0(implicit l0: => Link0): Link0 =
      new Link0 { lazy val next0 = l0 }
  }

  def check: Boolean = {
    val loop = implicitly[Link0]
    loop eq loop.next0
  }
}

object Loop1 {
  trait Link0 {
    def next0: Link0
    def next1: Link1
  }
  object Link0 {
    implicit def mkLink0(implicit l0: => Link0, l1: => Link1): Link0 =
      new Link0 { lazy val next0 = l0 ; lazy val next1 = l1 }
  }

  trait Link1 {
    def next0: Link0
    def next1: Link1
  }
  object Link1 {
    implicit def mkLink1(implicit l0: => Link0, l1: => Link1): Link1 =
      new Link1 { lazy val next0 = l0 ; lazy val next1 = l1 }
  }

  def check: Boolean = {
    val loop = implicitly[Link0]
    loop eq loop.next0
    loop.next1 eq loop.next1.next1
  }
}

object Loop2 {
  trait Link0 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
  }
  object Link0 {
    implicit def mkLink0(implicit l0: => Link0, l1: => Link1, l2: => Link2): Link0 =
      new Link0 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 }
  }

  trait Link1 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
  }
  object Link1 {
    implicit def mkLink1(implicit l0: => Link0, l1: => Link1, l2: => Link2): Link1 =
      new Link1 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 }
  }

  trait Link2 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
  }
  object Link2 {
    implicit def mkLink2(implicit l0: => Link0, l1: => Link1, l2: => Link2): Link2 =
      new Link2 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 }
  }

  def check: Boolean = {
    val loop = implicitly[Link0]
    loop eq loop.next0
    loop.next1 eq loop.next1.next1
    loop.next2 eq loop.next2.next2
  }
}

object Loop3 {
  trait Link0 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
    def next3: Link3
  }
  object Link0 {
    implicit def mkLink0(implicit l0: => Link0, l1: => Link1, l2: => Link2, l3: => Link3): Link0 =
      new Link0 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 ; lazy val next3 = l3 }
  }

  trait Link1 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
    def next3: Link3
  }
  object Link1 {
    implicit def mkLink1(implicit l0: => Link0, l1: => Link1, l2: => Link2, l3: => Link3): Link1 =
      new Link1 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 ; lazy val next3 = l3 }
  }

  trait Link2 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
    def next3: Link3
  }
  object Link2 {
    implicit def mkLink2(implicit l0: => Link0, l1: => Link1, l2: => Link2, l3: => Link3): Link2 =
      new Link2 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 ; lazy val next3 = l3 }
  }

  trait Link3 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
    def next3: Link3
  }
  object Link3 {
    implicit def mkLink3(implicit l0: => Link0, l1: => Link1, l2: => Link2, l3: => Link3): Link3 =
      new Link3 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 ; lazy val next3 = l3 }
  }

  def check: Boolean = {
    val loop = implicitly[Link0]
    loop eq loop.next0
    loop.next1 eq loop.next1.next1
    loop.next2 eq loop.next2.next2
    loop.next3 eq loop.next3.next3
  }
}

object Loop2b {
  trait Link0 {
    def next0: Link0
    def next1: Link1
    def next2: Link2
  }
  object Link0 {
    implicit def mkLink0(implicit l0: => Link0, l1: => Link1, l2: => Link2): Link0 =
      new Link0 { lazy val next0 = l0 ; lazy val next1 = l1 ; lazy val next2 = l2 }
  }

  trait Link1 {
    def next1: Link1
    def next2: Link2
  }
  object Link1 {
    implicit def mkLink1(implicit l1: => Link1, l2: => Link2): Link1 =
      new Link1 { lazy val next1 = l1 ; lazy val next2 = l2 }
  }

  trait Link2 {
    def next2: Link2
  }
  object Link2 {
    implicit def mkLink2(implicit l2: => Link2): Link2 =
      new Link2 { lazy val next2 = l2 }
  }

  def check: Boolean = {
    val loop = implicitly[Link0]
    loop eq loop.next0
    loop.next1 eq loop.next1.next1
    loop.next2 eq loop.next2.next2
  }
}

object Test extends App {
  assert(Loop0.check && Loop1.check && Loop2.check && Loop3.check && Loop2b.check)
}
