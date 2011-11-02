sealed trait Top
trait C {
  private object P extends Top
}                                                                                                                     
/*  
$ scala -e 'new AnyRef with C'
error: error while loading Top, class file '/private/tmp/bobobo/./Top.class' is broken
(error reading Scala signature of /private/tmp/bobobo/./Top.class: malformed Scala signature of Top at 185; reference value P of trait C refers to nonexisting symbol.)
one error found
*/
