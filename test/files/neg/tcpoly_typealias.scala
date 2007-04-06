trait A {
  type m[+x]
}

trait A2 {
  type m[+x <: String]
}

trait A3 {
  type m[x]
}

trait FooCov[+x]
trait FooCon[-x]
trait FooBound[+x <: String]

trait BOk1 extends A {
  type m[+x] = FooCov[x]
}

trait BOk2 extends A2 {
  type m[+x <: String] = FooBound[x]
}

trait BOk3 extends A2 {
  type m[+x] = FooCov[x] // weaker bound
}

trait BOk4 extends A3 {
  type m[+x] = FooCov[x] // weaker variance
}

// there are two aspects to check:
 // does type alias signature (not considering RHS) correspond to abstract type member in super class
 // does RHS correspond to the type alias sig
trait BInv extends A{
  type m[x] = FooCov[x] // error: invariant x in alias def
}

trait BCon extends A{
  type m[-x] = FooCon[x] // error: contravariant x
}

trait BBound extends A{
  type m[+x <: String] = FooBound[x] // error: x with stricter bound
}

