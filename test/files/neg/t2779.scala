abstract class M
{
    def f: List[M] = Nil
}

object M1 extends M

object M2 extends M
{
    override def f = List(M1)
}

object M3 extends M
{
    override def f = List(M1)
    override def f = List(M1)
}

object M4 extends M
{
    override def f = List(
        M3,
        M2
    )
}
