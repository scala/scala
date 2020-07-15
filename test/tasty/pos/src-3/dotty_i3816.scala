trait DottyIterable { self =>
  //type CC <: Iterable { type CC = self.CC }
  type DD[X] <: DottyIterable { type DD[Y] = self.DD[Y] }
}
