package monocle.std

import monocle.{Prism, PPrism, Iso, PIso}
import monocle.catssupport.Implicits._

object either extends EitherOptics

trait EitherOptics {
  
  final def pStdLeft[A, B, C]: PPrism[Either[A, B], Either[C, B], A, C] =
    PPrism[Either[A, B], Either[C, B], A, C]{
      case Left(a)  => \/.right(a)
      case Right(b) => \/.left(Right(b))
    }(Left.apply)

  final def stdLeft[A, B]: Prism[Either[A, B], A] =
    pStdLeft[A, B, A]

  final def pStdRight[A, B, C]: PPrism[Either[A, B], Either[A, C], B, C] =
    PPrism[Either[A, B], Either[A, C], B, C]{
      case Left(a)  => \/.left(Left(a))
      case Right(b) => \/.right(b)
    }(Right.apply)

  final def stdRight[A, B]: Prism[Either[A, B], B] =
    pStdRight[A, B, B]

  final def pEitherToDisjunction[E1, E2, A1, A2]: PIso[Either[E1, A1], Either[E2, A2], E1 \/ A1, E2 \/ A2] =
    disjunction.pDisjunctionToEither[E2, E1, A2, A1].reverse

  final def eitherToDisjunction[E, A]: Iso[Either[E, A], E \/ A] =
    pEitherToDisjunction[E, E, A, A]
}
