object Test extends App {
  type SA = { type U; type T; val f : T => (U, T) }
  type SB = { type U; type T; val g : T => (U, T) }

  type S = { type Utmp = this.b.type#U
    type Ttmp = this.a.type#T
    val a : (SA { type U = Utmp })
    val b : (SB { type T = Ttmp }) }

  val AB : S = new { self =>
    type Utmp = this.b.type#U
    type Ttmp = this.a.type#T
    val a : (SA { type U = self.type#Utmp }) = null
    val b : (SB { type T = self.type#Ttmp }) = null
  }
}
