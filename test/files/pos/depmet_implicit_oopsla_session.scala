object Sessions {
  trait Session[This] {
    type Dual
    type HasDual[D] = Session[This]{type Dual=D}
    def run(p: This, dp: Dual): Unit
  }

  implicit object StopSession extends Session[Stop] {
    type Dual = Stop

    def run(p: Stop, dp: Stop): Unit = {}
  }

  implicit def InDual[A, B](implicit sessionDIn: Session[B]) = 
    new Session[In[A, B]] {
      type Dual = Out[A, sessionDIn.Dual]

      def run(p: In[A, B], dp: Dual): Unit = 
        sessionDIn.run(p.func(dp.x), dp.y)
  }

  implicit def OutDual[A, B](implicit sessionDOut: Session[B]) = 
    new Session[Out[A, B]] {
     type Dual = In[A, sessionDOut.Dual]

     def run(p: Out[A, B], dp: Dual): Unit = 
       sessionDOut.run(p.y, dp.func(p.x))
  }

  sealed case class Stop()
  sealed case class In[-A, +B](func: A => B)
  sealed case class Out[+A, +B](x: A, y: B)

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

  def runSession[S, D: Session[S]#HasDual](p: S, dp: D) =
    implicitly[Session[S]#HasDual[D]].run(p, dp)

  // def runSession[S, D](p: S, dp: D)(implicit s: Session[S]#HasDual[D]) =
  //   s.run(p, dp)
  // 
  // def runSession[S, D](p: S, dp: D)(implicit s: Session[S]{type Dual=D}) =
  //   s.run(p, dp)

  // TODO: can we relax the ordering restrictions on dependencies so that we can use
  // def runSession[S](p: S, dp: s.Dual)(implicit s: Session[S]) =
  //   s.run(p, dp)
  // to emphasise similarity of type parameters and implicit arguments:
  // def runSession[S][val s: Session[S]](p: S, dp: s.Dual) =
  //   s.run(p, dp)

  def myRun = runSession(addServer, addClient)
}