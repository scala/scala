class foo {
      val wrong = <:bla/>
}
class bar {
      val wrong = <bla:  />
}

// this "pos" test is only included as a parser test
object pos
{
    def wrap(f : Int => Unit) = f(5)

    wrap({ v =>
        if(v == 5) {
            val n = {
                val m = (<a>{}</a>)
                <div>{ v }</div>
            }
            ()
        }
    })
}
