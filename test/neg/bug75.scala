class F[A]() {
    case class Nil[B]() {}

    val tree = null;
    protected def mkF[A](t:Nil[A]) =
	new F[A](){ override val tree=t;};

    def update = {
	mkF(Nil());
    }

}
