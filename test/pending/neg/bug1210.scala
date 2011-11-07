object Test {
  def id[T](f: T => T): T = error("bla")
  
  abstract class M { self =>
  	type Settings
  	type selfType = M {type Settings = self.Settings}

    val v: selfType = id[M.this.selfType](_.v)
  }
}
