package monocle.std

import monocle.{Iso, PIso, Prism, PPrism}
import monocle.catssupport.Implicits._

object disjunction extends DisjunctionOptics

trait DisjunctionOptics {
  
  final def pLeft[A, B, C]: PPrism[A \/ B, C \/ B, A, C] =
    PPrism[A \/ B, C \/ B, A, C](_.swap.bimap(\/.right, identity))(\/.left)

  final def left[A, B]: Prism[A \/ B, A] =
    pLeft[A, B, A]

  final def pRight[A, B, C]: PPrism[A \/ B, A \/ C, B, C] =
    PPrism[A \/ B, A \/ C, B, C](_.bimap(\/.left, identity))(\/.right)

  final def right[A, B]: Prism[A \/ B, B] =
    pRight[A, B, B]

  //final def pDisjunctionToValidation[E1, E2, A1, A2]: PIso[E1 \/ A1, E2 \/ A2, Validation[E1, A1], Validation[E2, A2]] =
  //  validation.pValidationToDisjunction[E2, E1, A2, A1].reverse

  //final def disjunctionToValidation[E, A]: Iso[E \/ A, Validation[E, A]] =
  //  pDisjunctionToValidation[E, E, A, A]

  final def pDisjunctionToEither[E1, E2, A1, A2]: PIso[E1 \/ A1, E2 \/ A2, Either[E1, A1], Either[E2, A2]] =
    PIso[E1 \/ A1, E2 \/ A2, Either[E1, A1], Either[E2, A2]](_.toEither)(_.fold(\/.left, \/.right))

  final def disjunctionToEither[E, A]: Iso[E \/ A, Either[E, A]] =
    pDisjunctionToEither[E, E, A, A]
}
