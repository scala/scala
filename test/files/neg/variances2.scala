trait Cov[+A]
trait Con[-A]
trait Inv[A]

trait Trait[-A, +B, C] {
  // trait Inner[-D <: C, +E >: C, F] {
  trait Inner[-D <: C, +E >: C, F] {
    def f0(x: A): Unit = ()
    def f1(x: B): Unit = ()
    def f2(x: C): Unit = ()
    def f3(x: D): Unit = ()
    def f4(x: E): Unit = ()
    def f5(x: F): Unit = ()

    def f6(): A = ???
    def f7(): B = ???
    def f8(): C = ???
    def f9(): D = ???
    def f10(): E = ???
    def f11(): F = ???

    def f12(f: A => A): Unit = ()
    def f13(f: A => B): Unit = ()
    def f14(f: A => C): Unit = ()
    def f15(f: A => D): Unit = ()
    def f16(f: A => E): Unit = ()
    def f17(f: A => F): Unit = ()
    def f18(f: B => A): Unit = ()
    def f19(f: B => B): Unit = ()
    def f20(f: B => C): Unit = ()
    def f21(f: B => D): Unit = ()
    def f22(f: B => E): Unit = ()
    def f23(f: B => F): Unit = ()
    def f24(f: C => A): Unit = ()
    def f25(f: C => B): Unit = ()
    def f26(f: C => C): Unit = ()
    def f27(f: C => D): Unit = ()
    def f28(f: C => E): Unit = ()
    def f29(f: C => F): Unit = ()
    def f30(f: D => A): Unit = ()
    def f31(f: D => B): Unit = ()
    def f32(f: D => C): Unit = ()
    def f33(f: D => D): Unit = ()
    def f34(f: D => E): Unit = ()
    def f35(f: D => F): Unit = ()
    def f36(f: E => A): Unit = ()
    def f37(f: E => B): Unit = ()
    def f38(f: E => C): Unit = ()
    def f39(f: E => D): Unit = ()
    def f40(f: E => E): Unit = ()
    def f41(f: E => F): Unit = ()
    def f42(f: F => A): Unit = ()
    def f43(f: F => B): Unit = ()
    def f44(f: F => C): Unit = ()
    def f45(f: F => D): Unit = ()
    def f46(f: F => E): Unit = ()
    def f47(f: F => F): Unit = ()

    def f48(): A => A = null
    def f49(): A => B = null
    def f50(): A => C = null
    def f51(): A => D = null
    def f52(): A => E = null
    def f53(): A => F = null
    def f54(): B => A = null
    def f55(): B => B = null
    def f56(): B => C = null
    def f57(): B => D = null
    def f58(): B => E = null
    def f59(): B => F = null
    def f60(): C => A = null
    def f61(): C => B = null
    def f62(): C => C = null
    def f63(): C => D = null
    def f64(): C => E = null
    def f65(): C => F = null
    def f66(): D => A = null
    def f67(): D => B = null
    def f68(): D => C = null
    def f69(): D => D = null
    def f70(): D => E = null
    def f71(): D => F = null
    def f72(): E => A = null
    def f73(): E => B = null
    def f74(): E => C = null
    def f75(): E => D = null
    def f76(): E => E = null
    def f77(): E => F = null
    def f78(): F => A = null
    def f79(): F => B = null
    def f80(): F => C = null
    def f81(): F => D = null
    def f82(): F => E = null
    def f83(): F => F = null

    def f84(x: A): A = ???
    def f85(x: A): B = ???
    def f86(x: A): C = ???
    def f87(x: A): D = ???
    def f88(x: A): E = ???
    def f89(x: A): F = ???
    def f90(x: B): A = ???
    def f91(x: B): B = ???
    def f92(x: B): C = ???
    def f93(x: B): D = ???
    def f94(x: B): E = ???
    def f95(x: B): F = ???
    def f96(x: C): A = ???
    def f97(x: C): B = ???
    def f98(x: C): C = ???
    def f99(x: C): D = ???
    def f100(x: C): E = ???
    def f101(x: C): F = ???
    def f102(x: D): A = ???
    def f103(x: D): B = ???
    def f104(x: D): C = ???
    def f105(x: D): D = ???
    def f106(x: D): E = ???
    def f107(x: D): F = ???
    def f108(x: E): A = ???
    def f109(x: E): B = ???
    def f110(x: E): C = ???
    def f111(x: E): D = ???
    def f112(x: E): E = ???
    def f113(x: E): F = ???
    def f114(x: F): A = ???
    def f115(x: F): B = ???
    def f116(x: F): C = ???
    def f117(x: F): D = ???
    def f118(x: F): E = ???
    def f119(x: F): F = ???

    object O1 extends Cov[A]
    object O2 extends Cov[B]
    object O3 extends Cov[C]
    object O4 extends Cov[D]
    object O5 extends Cov[E]
    object O6 extends Cov[F]
    object O7 extends Con[A]
    object O8 extends Con[B]
    object O9 extends Con[C]
    object O10 extends Con[D]
    object O11 extends Con[E]
    object O12 extends Con[F]
    object O13 extends Inv[A]
    object O14 extends Inv[B]
    object O15 extends Inv[C]
    object O16 extends Inv[D]
    object O17 extends Inv[E]
    object O18 extends Inv[F]
  }
}

trait Trait2[-A, +B, C] {
  // trait Inner[-D <: C, +E >: C, F] {
  def method[D <: A, E >: B, F]() {
    def f0(x: A): Unit = ()
    def f1(x: B): Unit = ()
    def f2(x: C): Unit = ()
    def f3(x: D): Unit = ()
    def f4(x: E): Unit = ()
    def f5(x: F): Unit = ()

    def f6(): A = ???
    def f7(): B = ???
    def f8(): C = ???
    def f9(): D = ???
    def f10(): E = ???
    def f11(): F = ???

    def f12(f: A => A): Unit = ()
    def f13(f: A => B): Unit = ()
    def f14(f: A => C): Unit = ()
    def f15(f: A => D): Unit = ()
    def f16(f: A => E): Unit = ()
    def f17(f: A => F): Unit = ()
    def f18(f: B => A): Unit = ()
    def f19(f: B => B): Unit = ()
    def f20(f: B => C): Unit = ()
    def f21(f: B => D): Unit = ()
    def f22(f: B => E): Unit = ()
    def f23(f: B => F): Unit = ()
    def f24(f: C => A): Unit = ()
    def f25(f: C => B): Unit = ()
    def f26(f: C => C): Unit = ()
    def f27(f: C => D): Unit = ()
    def f28(f: C => E): Unit = ()
    def f29(f: C => F): Unit = ()
    def f30(f: D => A): Unit = ()
    def f31(f: D => B): Unit = ()
    def f32(f: D => C): Unit = ()
    def f33(f: D => D): Unit = ()
    def f34(f: D => E): Unit = ()
    def f35(f: D => F): Unit = ()
    def f36(f: E => A): Unit = ()
    def f37(f: E => B): Unit = ()
    def f38(f: E => C): Unit = ()
    def f39(f: E => D): Unit = ()
    def f40(f: E => E): Unit = ()
    def f41(f: E => F): Unit = ()
    def f42(f: F => A): Unit = ()
    def f43(f: F => B): Unit = ()
    def f44(f: F => C): Unit = ()
    def f45(f: F => D): Unit = ()
    def f46(f: F => E): Unit = ()
    def f47(f: F => F): Unit = ()

    def f48(): A => A = null
    def f49(): A => B = null
    def f50(): A => C = null
    def f51(): A => D = null
    def f52(): A => E = null
    def f53(): A => F = null
    def f54(): B => A = null
    def f55(): B => B = null
    def f56(): B => C = null
    def f57(): B => D = null
    def f58(): B => E = null
    def f59(): B => F = null
    def f60(): C => A = null
    def f61(): C => B = null
    def f62(): C => C = null
    def f63(): C => D = null
    def f64(): C => E = null
    def f65(): C => F = null
    def f66(): D => A = null
    def f67(): D => B = null
    def f68(): D => C = null
    def f69(): D => D = null
    def f70(): D => E = null
    def f71(): D => F = null
    def f72(): E => A = null
    def f73(): E => B = null
    def f74(): E => C = null
    def f75(): E => D = null
    def f76(): E => E = null
    def f77(): E => F = null
    def f78(): F => A = null
    def f79(): F => B = null
    def f80(): F => C = null
    def f81(): F => D = null
    def f82(): F => E = null
    def f83(): F => F = null

    def f84(x: A): A = ???
    def f85(x: A): B = ???
    def f86(x: A): C = ???
    def f87(x: A): D = ???
    def f88(x: A): E = ???
    def f89(x: A): F = ???
    def f90(x: B): A = ???
    def f91(x: B): B = ???
    def f92(x: B): C = ???
    def f93(x: B): D = ???
    def f94(x: B): E = ???
    def f95(x: B): F = ???
    def f96(x: C): A = ???
    def f97(x: C): B = ???
    def f98(x: C): C = ???
    def f99(x: C): D = ???
    def f100(x: C): E = ???
    def f101(x: C): F = ???
    def f102(x: D): A = ???
    def f103(x: D): B = ???
    def f104(x: D): C = ???
    def f105(x: D): D = ???
    def f106(x: D): E = ???
    def f107(x: D): F = ???
    def f108(x: E): A = ???
    def f109(x: E): B = ???
    def f110(x: E): C = ???
    def f111(x: E): D = ???
    def f112(x: E): E = ???
    def f113(x: E): F = ???
    def f114(x: F): A = ???
    def f115(x: F): B = ???
    def f116(x: F): C = ???
    def f117(x: F): D = ???
    def f118(x: F): E = ???
    def f119(x: F): F = ???

    object O1 extends Cov[A]
    object O2 extends Cov[B]
    object O3 extends Cov[C]
    object O4 extends Cov[D]
    object O5 extends Cov[E]
    object O6 extends Cov[F]
    object O7 extends Con[A]
    object O8 extends Con[B]
    object O9 extends Con[C]
    object O10 extends Con[D]
    object O11 extends Con[E]
    object O12 extends Con[F]
    object O13 extends Inv[A]
    object O14 extends Inv[B]
    object O15 extends Inv[C]
    object O16 extends Inv[D]
    object O17 extends Inv[E]
    object O18 extends Inv[F]

    ()
  }
}
