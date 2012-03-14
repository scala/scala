object Test {
    abstract class Settings {}

    abstract class Grist
    { self =>
        type settingsType <: Settings
        type moduleType <: Module {type settingsType = self.settingsType}
        val module: moduleType
    }

    abstract class Tool
    { self =>
        type settingsType <: Settings
        type moduleType = Module { type settingsType = self.settingsType }
        type gristType = Grist { type moduleType <: self.moduleType; type settingsType <: self.settingsType }

        def inputGrist: List[gristType]
    }

    abstract class Module
    { self =>
        type settingsType <: Settings
        final type commonModuleType = Module {type settingsType = self.settingsType}
        type selfType >: self.type <: commonModuleType

        // BTW: if we use the commented out type declarations, the code compiles successfully
        // type gristType = Grist {type settingsType <: self.settingsType; type moduleType <: commonModuleType }

        val tools: List[Tool {type settingsType = self.settingsType}]

        protected def f: List[commonModuleType] =
        {
            val inputGrists = tools.flatMap(_.inputGrist) // val inputGrists: List[gristType] =
            inputGrists.map(_.module)
        }

    }
}
