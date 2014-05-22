class DuplicateClassName {
  () => {
    {() => ()}
    {() => ()}
    {() => ()}
    {() => ()}
    {() => ()}
    {() => ()}
    {() => ()}
    {() => ()}
    {() => ()}
    {() => () => ()}
    {() => ()}
  }
}
// Was:
// Different class symbols have the same bytecode-level internal name:
// name: DuplicateClassName$lambda$$$anonfun$111