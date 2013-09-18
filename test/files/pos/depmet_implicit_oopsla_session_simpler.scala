object Sessions {
  trait Session {
    type Dual <: Session

    def run(dp: Dual): Unit
  }

  sealed case class Stop() extends Session {
    type Dual = Stop

    def run(dp: Dual): Unit = {}
  }

  // can't write B <: Session{type Dual = BDual} due to limitations in type inference algorithm
  // (type variables cannot occur on both sides of <:)
  // using B#Dual instead of BDual is too imprecise, since it is disconnected from the actual argument that is passed for B
  // would be nice if we could introduce a universal quantification over BDual that is not part of the
  // type parameter list
  sealed case class In[A, B <: Session, BDual <: Session](recv: A => B)(implicit dual: B <:< Session{type Dual=BDual}) extends Session {
    type Dual = Out[A, BDual]

    def run(dp: Dual): Unit = recv(dp.data) run dp.cont
  }

  sealed case class Out[A, B <: Session](data: A, cont: B) extends Session {
     type Dual = In[A, cont.Dual, cont.Dual#Dual]

     def run(dp: Dual): Unit = cont run dp.recv(data)
  }

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

  def myRun = addServer run addClient
}
