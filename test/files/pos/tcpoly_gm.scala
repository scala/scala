trait Rep[a] {           
  def rep[m[x]]: m[a] // typedTypeApply must use asSeenFrom to adapt the return type
   // since rep is called on x: Rep[t]
   // a must become t
}

case class ShowBin[b](app: b => String)

object foo {
  def showBin[t](x: Rep[t], y: t): String = {
    val r: ShowBin[t] = x.rep[ShowBin]
    r.app(y) 
  }
}
 
