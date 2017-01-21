import sbt.{Plugin, SettingKey}

object Settings extends Plugin {
  sealed trait TypeClassLibrary
  case object Cats extends TypeClassLibrary {
    override def toString: String = "cats"
  }
  case object Scalaz extends TypeClassLibrary {
    override def toString: String = "scalaz"
  }

  val typeclassLibrary = SettingKey[TypeClassLibrary]("typeclassLibrary", "typeclass library to use (either Cats or Scalaz)")
}