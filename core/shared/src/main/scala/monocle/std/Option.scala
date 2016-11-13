package monocle.std

import monocle.{Iso, PIso, PPrism, Prism}

import monocle.catssupport.Implicits._

object option extends OptionOptics

trait OptionOptics {
  final def pSome[A, B]: PPrism[Option[A], Option[B], A, B] =
    PPrism[Option[A], Option[B], A, B](_.map(\/.right(_)) getOrElse \/.left(None))(Some.apply)

  final def some[A]: Prism[Option[A], A] =
    pSome[A, A]

  final def none[A]: Prism[Option[A], Unit] =
    Prism[Option[A], Unit]{ case None => Some(()); case Some(_) => None }(_ => None)

  final def pOptionToDisjunction[A, B]: PIso[Option[A], Option[B], Unit \/ A, Unit \/ B] =
    PIso[Option[A], Option[B], Unit \/ A, Unit \/ B](_.map(\/.right(_)) getOrElse \/.left(()))(_.toOption)

  final def optionToDisjunction[A]: Iso[Option[A], Unit \/ A] =
    pOptionToDisjunction[A, A]
}

