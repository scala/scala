// This one is weird and nasty. Not sure if this is scalac or sbt
// (tried with 0.12 & 0.12.2-RC2) bug.
//
// A level of indirection is required to trigger this bug.
// Exts seems to need to be defined in separate file.
//
// Steps to reproduce:
// 1. sbt clean
// 2. sbt run (it works)
// 3. Comment A & uncomment B.
// 4. sbt run (it fails)
// 5. Switch it back & sbt run. It still fails.
//
// In this project sbt clean helps. However in a large project where this
// bug was found compiler crashed even after doing sbt clean. The only
// way to work around this was to reference Exts object explicitly (C) in
// the source file using its implicit classes.

// Lets suppose this is a mega-trait combining all sorts of helper
// functionality.
trait Support extends Exts

object ImplicitsBug extends App with Support { // A
// object ImplicitsBug extends App with Exts { // B
  //Exts // C) this reference helped in the large project.
  println(3.moo)
}
