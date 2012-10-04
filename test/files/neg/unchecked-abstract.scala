trait Contravariant[-X]
trait Invariant[X]
trait Covariant[+X]

abstract class M {
  type H
  type L <: H
  type T >: L <: H

  def h1(x: Contravariant[H]) = {
    /* nowarn */ println(x.isInstanceOf[Contravariant[H]])
    /* nowarn */ println(x.isInstanceOf[Contravariant[T]])
    /* nowarn */ println(x.isInstanceOf[Contravariant[L]])
  }
  def h2(x: Contravariant[T]) = {
    /*   warn */ println(x.isInstanceOf[Contravariant[H]])
    /* nowarn */ println(x.isInstanceOf[Contravariant[T]])
    /* nowarn */ println(x.isInstanceOf[Contravariant[L]])
  }
  def h3(x: Contravariant[L]) = {
    /*   warn */ println(x.isInstanceOf[Contravariant[H]])
    /*   warn */ println(x.isInstanceOf[Contravariant[T]])
    /* nowarn */ println(x.isInstanceOf[Contravariant[L]])
  }
  def h4(x: Invariant[H]) = {
    /* nowarn */ println(x.isInstanceOf[Invariant[H]])
    /*   warn */ println(x.isInstanceOf[Invariant[T]])
    /*   warn */ println(x.isInstanceOf[Invariant[L]])
  }
  def h5(x: Invariant[T]) = {
    /*   warn */ println(x.isInstanceOf[Invariant[H]])
    /* nowarn */ println(x.isInstanceOf[Invariant[T]])
    /*   warn */ println(x.isInstanceOf[Invariant[L]])
  }
  def h6(x: Invariant[L]) = {
    /*   warn */ println(x.isInstanceOf[Invariant[H]])
    /*   warn */ println(x.isInstanceOf[Invariant[T]])
    /* nowarn */ println(x.isInstanceOf[Invariant[L]])
  }
  def h7(x: Covariant[H]) = {
    /* nowarn */ println(x.isInstanceOf[Covariant[H]])
    /*   warn */ println(x.isInstanceOf[Covariant[T]])
    /*   warn */ println(x.isInstanceOf[Covariant[L]])
  }
  def h8(x: Covariant[T]) = {
    /* nowarn */ println(x.isInstanceOf[Covariant[H]])
    /* nowarn */ println(x.isInstanceOf[Covariant[T]])
    /*   warn */ println(x.isInstanceOf[Covariant[L]])
  }
  def h9(x: Covariant[L]) = {
    /* nowarn */ println(x.isInstanceOf[Covariant[H]])
    /* nowarn */ println(x.isInstanceOf[Covariant[T]])
    /* nowarn */ println(x.isInstanceOf[Covariant[L]])
  }
}

object Test extends M {
  type H = Any
  type T = Int
  type L = Nothing

  val conh = new Contravariant[H] { }
  val cont = new Contravariant[T] { }
  val conl = new Contravariant[L] { }

  val invh = new Invariant[H] { }
  val invt = new Invariant[T] { }
  val invl = new Invariant[L] { }

  val covh = new Covariant[H] { }
  val covt = new Covariant[T] { }
  val covl = new Covariant[L] { }

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
