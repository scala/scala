// tested using Scala compiler version 2.6.0-RC1 -- (c) 2002-2010 LAMP/EPFL

// prompted by "Covariant return types" mailing list question
object TestCovariance {

    // see  Type constructor polymorphism  in  http://www.scala-lang.org/docu/changelog.html
    trait Seq[+t] {
        type MyType[+t] <: Seq[t]

        def f: MyType[t]
    }

    def span[a, s <: Seq[a] { type MyType <: s } ](xs: s): s = xs f
}
