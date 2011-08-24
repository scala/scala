trait B
{
     type T >: this.type <: B


     // compile-time check: have we achieved our objective?
     def test: T = this
}


