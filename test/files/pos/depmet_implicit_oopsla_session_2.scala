object Sessions {
  def ?[T <: AnyRef](implicit w: T): w.type = w

  // session states
  sealed case class Stop()
  sealed case class In[-Data, +Cont](recv: Data => Cont)
  sealed case class Out[+Data, +Cont](data: Data, cont: Cont)

  // the type theory of communicating sessions:

  // an instance of type Session[S]{type Dual=D} is evidence that S and D are duals
  // such a value witnesses this fact by describing how to compose an instance of S with an instance of D (through the run method)
  trait Session[S] { type Self = S
    type Dual
    type HasDual[D] = Session[Self]{type Dual=D}
    def run(self: Self, dual: Dual): Unit
  }

  // friendly interface to the theory
  def runSession[S, D: Session[S]#HasDual](session: S, dual: D) =
    ?[Session[S]#HasDual[D]].run(session, dual)

  // facts in the theory:

  // ------------------------[StopDual]
  // Stop is the dual of Stop
  implicit object StopDual extends Session[Stop] {
    type Dual = Stop

    def run(self: Self, dual: Dual): Unit = {}
  }

  //            CD is the dual of Cont
  // -------------------------------------------[InDual]
  // Out[Data, CD] is the dual of In[Data, Cont]
  implicit def InDual[Data, Cont](implicit cont: Session[Cont]) = new Session[In[Data, Cont]] {
    type Dual = Out[Data, cont.Dual]

    def run(self: Self, dual: Dual): Unit =
      cont.run(self.recv(dual.data), dual.cont)
  }

  //            CD is the dual of Cont
  // -------------------------------------------[OutDual]
  // In[Data, CD] is the dual of Out[Data, Cont]
  implicit def OutDual[Data, Cont](implicit cont: Session[Cont]) = new Session[Out[Data, Cont]] {
    type Dual = In[Data, cont.Dual]

    def run(self: Self, dual: Dual): Unit =
      cont.run(self.cont, dual.recv(self.data))
  }

  // a concrete session
  def addServer =
    In{x: Int =>
    In{y: Int => System.out.println("Thinking")
    Out(x+y,
    Stop())}}

  def addClient =
    Out(3,
    Out(4, { System.out.println("Waiting")
    In{z: Int => System.out.println(z)
    Stop()}}))

  def myRun = runSession(addServer, addClient)
}

/* future improvements:


  // def runSession[S, D](p: S, dp: D)(implicit s: Session[S]#HasDual[D]) =
  //   s.run(p, dp)
  //
  // def runSession[S, D](p: S, dp: D)(implicit s: Session[S]{type Dual=D}) =
  //   s.run(p, dp)

  // TODO: can we relax the ordering restrictions on dependencies so that we can write
  //  one possibility: graph of dependencies between arguments must be acyclic
  // def runSession[S](p: S, dp: s.Dual)(implicit s: Session[S]) =
  //   s.run(p, dp)
  // to emphasise similarity of type parameters and implicit arguments:
  // def runSession[S][val s: Session[S]](p: S, dp: s.Dual) =
  //   s.run(p, dp)


*/