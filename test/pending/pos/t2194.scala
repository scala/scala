scala> class C
defined class C

scala> def f = { object o extends C; o}
f: ewo.type forSome { val o: o; type o <: C with ScalaObject }

scala> val x = f
<console>:6: error: type mismatch;
 found   : o.type(in object $iw) where type o.type(in object $iw) <: o with Singleton
 required: o.type(in value x) forSome { type o.type(in value x) <: o with Singleton; type o <: C with ScalaObject }
       val x = f
           ^

scala> val x : C = f
x: C = o$2$@111985e
