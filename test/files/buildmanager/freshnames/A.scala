abstract class A {

    var t: List[B]
    
    def foo(n: String): Option[B] = {
        t.reverse find (_.names contains n)
    }
    
    def bar(n: Int): Option[B] = {
        t.reverse find (_.names contains n)
    }
}

//class A
case class B(names: List[String])

