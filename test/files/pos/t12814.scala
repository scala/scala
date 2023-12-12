// https://github.com/scala/bug/issues/12814#issuecomment-1822770100
object t1 {
  trait A[X] { type T = X }
  object B extends A[String]
  object C extends A[B.T] {
    def f: C.T = "hai"
  }
}

// https://github.com/scala/bug/issues/12814
object t2 {
  sealed trait Common
  sealed trait One extends Common
  sealed trait Two extends Common


  trait Module[C <: Common] {
    val name: String
    type Narrow = C
    def narrow: PartialFunction[Common, C]
  }

  object ModuleA extends Module[One] {
    val name = "A"
    val narrow: PartialFunction[Common, Narrow] = {
      case cc: Narrow => cc
    }
  }

  object ModuleB extends Module[ModuleA.Narrow] {
    val name = "B"
    val narrow: PartialFunction[Common, Narrow] = {
      case cc: Narrow => cc
    }
  }

  object ModuleC extends Module[Two] {
    val name = "C"
    val narrow: PartialFunction[Common, Narrow] = {
      case cc: Narrow => cc
    }
  }

  object ModuleD extends Module[One with Two] {
    val name = "D"
    val narrow: PartialFunction[Common, Narrow] = {
      case cc: Narrow => cc
    }
  }

  val one    = new One {}
  val two    = new Two {}
  val oneTwo = new One with Two {}

  Seq(ModuleA, ModuleB, ModuleC, ModuleD).foreach { module =>
    println(s"${module.name} at One    = ${module.narrow.isDefinedAt(one)}")
    println(s"${module.name} at Two    = ${module.narrow.isDefinedAt(two)}")
    println(s"${module.name} at OneTwo = ${module.narrow.isDefinedAt(oneTwo)}")
    println("-" * 10)
  }
}

// https://github.com/scala/scala/pull/10457/files
object t3 {
  sealed trait A

  sealed trait B extends A

  trait F[C] {
    type T = C
  }

  object O extends F[B]

  object P1 extends F[O.T] {
    val f: PartialFunction[A, P1.T] = {
      case x: P1.T => x
    }
  }

  object P2 extends F[O.T] {
    val f: PartialFunction[A, P2.T] = x => x match {
      case x: P2.T => x
    }
  }

  object P3 extends F[O.T] {
    val f: Function1[A, P3.T] = {
      case x: P3.T => x
    }
  }

  object P4 extends F[O.T] {
    val f: Function1[A, P4.T] = x => x match {
      case x: P4.T => x
    }
  }
}