object Test extends App{
  val q : Queryable[Any] = new Queryable[Any]
  q.map(e1 => q.map(e2=>e1).map(e2=>e1))

  locally {
    val q : Queryable[Any] = new Queryable[Any]
    q.map(e1 => q.map(e2=>e1).map(e2=>e1))
  }
}