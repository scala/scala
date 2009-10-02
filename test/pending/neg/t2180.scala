

Given the following code (which is broken):

class Mxml {

    private def processChildren( children:Seq[Any] ):List[Mxml] = {

        children.toList.flatMap ( e => {

            e match {

                case s:scala.collection.Traversable[_] => s case a => List(a)

            }

        })

    }

}

I get the following error:

Mxml.scala:5: error: could not find implicit value for parameter bf:scala.collection.generic.BuilderFactory[Any,List[Mxml],Sequence[Any]].

    children.flatMap ( e => {

I spent 4 hours failing before I figured out the problem. The return type was wrong. It should have been List[Any].

I have seen similar errors with map. My solution in the past has been to change it to a foldLeft because I have never been able to determine how to fix the problem until now.
