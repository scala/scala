/* This program puts the compiler into an endless loop
*/
trait T1 {
   type T;
}
trait T2 {
    type S;
    type T <: S;
}
abstract class S6() extends T1 with T2 {
    type S <: T;
}
