object testCQ  {
                            // why does this not work directly
  case class Thing( name:String, contains:List[ Thing ]  );

  /* ... but this one does?
  abstract class T;
  case class Thing2( name:String, contains:List[ T ]  ) extends T;
  */

}
