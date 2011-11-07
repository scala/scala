object Test {
  for {
    n <- Some(42)

    _ 
    m <- Some(24)
  } yield n
}
