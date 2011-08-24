object test {
  for {
    n <- Some(42)
    _ <- Some(24)
  } yield n
}
