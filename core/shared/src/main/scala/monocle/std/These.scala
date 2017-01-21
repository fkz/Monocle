package monocle.std

import monocle.{OnlyInScalaz, Prism}

//import scalaz.\&/.{Both, That, This}
//import scalaz.syntax.either._
//import scalaz.{-\/, \&/, \/, \/-}

object these extends TheseOptics

trait TheseOptics {
  @OnlyInScalaz
  def theseToDisjunction[A, B]: Prism[A \&/ B, A \/ B] = Prism[A \&/ B, A \/ B]{
    case This(a) => Some(a.left[B])
    case That(b) => Some(b.right[A])
    case Both(_, _) => None
  }{
    case -\/(a) => This(a)
    case \/-(b) => That(b)
  }

  @OnlyInScalaz
  @deprecated("use theseToDisjunction", since = "1.2.0")
  def theseDisjunction[A, B]: Prism[A \&/ B, A \/ B] =
    theseToDisjunction[A, B]
}
