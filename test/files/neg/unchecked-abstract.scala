trait Con[-X]
trait Inv[X]
trait Cov[+X]

abstract class M {
  type H
  type L <: H
  type T >: L <: H

  def h1(x: Con[H]) = {
    /* nowarn */ println(x.isInstanceOf[Con[H]])
    /* nowarn */ println(x.isInstanceOf[Con[T]])
    /* nowarn */ println(x.isInstanceOf[Con[L]])
  }
  def h2(x: Con[T]) = {
    /*   warn */ println(x.isInstanceOf[Con[H]])
    /* nowarn */ println(x.isInstanceOf[Con[T]])
    /* nowarn */ println(x.isInstanceOf[Con[L]])
  }
  def h3(x: Con[L]) = {
    /*   warn */ println(x.isInstanceOf[Con[H]])
    /*   warn */ println(x.isInstanceOf[Con[T]])
    /* nowarn */ println(x.isInstanceOf[Con[L]])
  }
  def h4(x: Inv[H]) = {
    /* nowarn */ println(x.isInstanceOf[Inv[H]])
    /*   warn */ println(x.isInstanceOf[Inv[T]])
    /*   warn */ println(x.isInstanceOf[Inv[L]])
  }
  def h5(x: Inv[T]) = {
    /*   warn */ println(x.isInstanceOf[Inv[H]])
    /* nowarn */ println(x.isInstanceOf[Inv[T]])
    /*   warn */ println(x.isInstanceOf[Inv[L]])
  }
  def h6(x: Inv[L]) = {
    /*   warn */ println(x.isInstanceOf[Inv[H]])
    /*   warn */ println(x.isInstanceOf[Inv[T]])
    /* nowarn */ println(x.isInstanceOf[Inv[L]])
  }
  def h7(x: Cov[H]) = {
    /* nowarn */ println(x.isInstanceOf[Cov[H]])
    /*   warn */ println(x.isInstanceOf[Cov[T]])
    /*   warn */ println(x.isInstanceOf[Cov[L]])
  }
  def h8(x: Cov[T]) = {
    /* nowarn */ println(x.isInstanceOf[Cov[H]])
    /* nowarn */ println(x.isInstanceOf[Cov[T]])
    /*   warn */ println(x.isInstanceOf[Cov[L]])
  }
  def h9(x: Cov[L]) = {
    /* nowarn */ println(x.isInstanceOf[Cov[H]])
    /* nowarn */ println(x.isInstanceOf[Cov[T]])
    /* nowarn */ println(x.isInstanceOf[Cov[L]])
  }
}

object Test extends M {
  type H = Any
  type T = Int
  type L = Nothing

  val conh = new Con[H] { }
  val cont = new Con[T] { }
  val conl = new Con[L] { }

  val invh = new Inv[H] { }
  val invt = new Inv[T] { }
  val invl = new Inv[L] { }

  val covh = new Cov[H] { }
  val covt = new Cov[T] { }
  val covl = new Cov[L] { }

  def main(args: Array[String]): Unit = {
    h1(conh)
    h2(conh)
    h2(cont)
    h3(conh)
    h3(cont)
    h3(conl)

    h4(invh)
    h5(invt)
    h6(invl)

    h7(covh)
    h7(covt)
    h7(covl)
    h8(covt)
    h8(covl)
    h9(covl)
  }
}
