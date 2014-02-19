object Test extends App{
  val q : Queryable[Any] = new Queryable[Any]
  q.map(x => x).map(x => x)

  locally {
    val q : Queryable[Any] = new Queryable[Any]
    q.map(x => x).map(x => x)
  }
}