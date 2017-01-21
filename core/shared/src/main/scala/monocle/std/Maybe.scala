package monocle.std

import monocle._
import cats.syntax.option._
import monocle.catssupport.Implicits._

object maybe extends MaybeOptics

trait MaybeOptics {
  @OnlyInScalaz
  final def pMaybeToOption[A, B]: PIso[Maybe[A], Maybe[B], Option[A], Option[B]] =
    PIso((_: Maybe[A]).toOption)((_: Option[B]).toMaybe)

  @OnlyInScalaz
  final def maybeToOption[A]: Iso[Maybe[A], Option[A]] =
    pMaybeToOption[A, A]

  @OnlyInScalaz
  final def pJust[A, B]: PPrism[Maybe[A], Maybe[B], A, B] =
    PPrism[Maybe[A], Maybe[B], A, B](_.cata(\/-(_), -\/(Maybe.empty)))(Maybe.just[B])

  @OnlyInScalaz
  final def just[A]: Prism[Maybe[A], A] =
    pJust[A, A]

  final def nothing[A]: Prism[Maybe[A], Unit] =
    Prism[Maybe[A], Unit](m => if(m.isEmpty) Some(()) else None)(_ => Maybe.empty)
}