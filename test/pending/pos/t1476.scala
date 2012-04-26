abstract class Module {
  def moduleDemands(): List[Module]
}

object Test {
  new Module { owner: Module =>
    def moduleDemands() = Nil

    val a = new Module { def moduleDemands(): List[Module] = Nil }
    val b = new Module { def moduleDemands(): List[Module] = owner :: c :: Nil }
    val c = new Module { def moduleDemands(): List[Module] = owner :: a :: Nil }
  }
}

object Test2 {
  new Module { owner =>
    def moduleDemands() = Nil

    val a = new Module { def moduleDemands(): List[Module] = Nil }
    val b = new Module { def moduleDemands(): List[Module] = owner :: c :: Nil }
    val c = new Module { def moduleDemands(): List[Module] = owner :: a :: Nil }
  }
}
