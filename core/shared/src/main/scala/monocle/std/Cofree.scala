package monocle.std

import monocle.{Iso, OnlyInScalaz, PIso}
import cats.data.OneAnd

object cofree extends CofreeOptics

trait CofreeOptics {

  /** Polymorphic isomorphism between `Cofree[Option, _]` and `OneAnd[Stream, _]` */
  @OnlyInScalaz def pCofreeToStream[A, B]: PIso[Cofree[Option, A], Cofree[Option, B],
                                  OneAnd[Stream, A], OneAnd[Stream, B]] =
    PIso[Cofree[Option, A], Cofree[Option, B], OneAnd[Stream, A], OneAnd[Stream, B]](
      (c: Cofree[Option, A]) => OneAnd[Stream, A](c.head, toStream(c.tail))
    ){ case OneAnd(head, tail) => fromStream(head, tail) }

  /** [[Iso]] variant of [[pCofreeToStream]]  */
  @OnlyInScalaz def cofreeToStream[A]: Iso[Cofree[Option, A], OneAnd[Stream, A]] =
    pCofreeToStream[A, A]


  @OnlyInScalaz private def toTree[A](c: Cofree[Stream, A]): Tree[A] =
    Node(c.head, c.tail.map(toTree[A]))

  @OnlyInScalaz private def fromTree[A](c: Tree[A]): Cofree[Stream, A] =
    Cofree.delay(c.rootLabel, c.subForest.map(fromTree[A]))

  /** Polymorphic isomorphism between `Cofree[Stream, _]` and `Tree` */
  @OnlyInScalaz def pCofreeToTree[A, B]: PIso[Cofree[Stream, A], Cofree[Stream, B],
                                Tree[A], Tree[B]] =
    PIso[Cofree[Stream, A], Cofree[Stream, B], Tree[A], Tree[B]](toTree[A])(fromTree[B])

  /** [[Iso]] variant of [[pCofreeToTree]] */
  @OnlyInScalaz def cofreeToTree[A]: Iso[Cofree[Stream, A], Tree[A]] =
    pCofreeToTree[A, A]

  @OnlyInScalaz private def toStream[A](optC: Option[Cofree[Option, A]]): Stream[A] =
    optC.fold(Stream.empty[A])(c => c.head #:: toStream(c.tail))

  @OnlyInScalaz private def fromStream[A, B](z: A, c: Stream[A]): Cofree[Option, A] = c match {
    case head #:: tail => Cofree.delay(z, Some(fromStream(head, tail)))
    case _ => Cofree(z, None: Option[Cofree[Option, A]])
  }
}
