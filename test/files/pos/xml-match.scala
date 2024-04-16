//> using options -Ystop-after:parser
//
object foo {
  def bar(e: Elem) = e match {
    case <t>{ _* }</t> => <q/>
    case <t>{ _ }</t> => <q/>
    case <t>{{3}}</t> => <t>{{3}}</t>
    case <t> {{3}}</t> => <t> {{3}}</t>
    case <version>{ x }</version> if x.toString.toInt < 4 =>
      <version>{ x.toString.toInt + 1 }</version>
  }
  def bar(n: Node) = n match {
    case <t>{ _* }</t> => <q/>
    case <t>{ _ }</t> => <q/>
    case <t>{{3}}</t> => <t>{{3}}</t>
    case <t> {{3}} </t> => <t> {{3}} </t>
    case <version>{ x }</version> if x.toString.toInt < 4 =>
      <version>{ x.toString.toInt + 1 }</version>
  }
}
