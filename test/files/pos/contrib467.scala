abstract class M
{
    val data: List[M]
}

object Test
{
    def main(args: Array[String]): Unit =
    {
        object SA extends M
        {
            val data = List()
        }

        object SB extends M
        {
            val data = List(SA)
        }

        ()
    }
}
