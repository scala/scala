abstract class C { type T; val next: C = this }

object test {

    val x: C = new C { type T = int };
    var y: C = x;
    def z: C = x;

    type a = x.T;
    type b = y.T;
    type c = z.T;
    type d = x.next.next.T;

    import x.T;

    type e = T;
}
