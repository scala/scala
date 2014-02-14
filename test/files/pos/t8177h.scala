class Module { self =>
  type settingsType <: Any
  final type commonModuleType = Module {type settingsType = self.settingsType}
  def foo(s: self.type): commonModuleType = s
}
