// no warning here
// (strangely, if there's an unreachable code warning *anywhere in this compilation unit*,
//  the non-sensical warning goes away under -Xfatal-warnings)
class Test {
  () match { case () => }
}
