package monocle.law

import monocle.{OnlyInCats, OnlyInScalaz, Optional}
import monocle.internal.IsEq
import cats.{catsInstancesForId => id}
import cats.instances.option._
import cats.syntax.option._
import cats.data.Const
import monocle.catssupport.First
import monocle.catssupport.Implicits._

case class OptionalLaws[S, A](optional: Optional[S, A]) {
  import IsEq.syntax

  def getOptionSet(s: S): IsEq[S] =
    optional.getOrModify(s).fold(identity, optional.set(_)(s)) <==> s

  def setGetOption(s: S, a: A): IsEq[Option[A]] =
    optional.getOption(optional.set(a)(s)) <==> optional.getOption(s).map(_ => a)

  def setIdempotent(s: S, a: A): IsEq[S] =
    optional.set(a)(optional.set(a)(s)) <==> optional.set(a)(s)

  def modifyIdentity(s: S): IsEq[S] =
    optional.modify(identity)(s) <==> s

  def composeModify(s: S, f: A => A, g: A => A): IsEq[S] =
    optional.modify(g)(optional.modify(f)(s)) <==> optional.modify(g compose f)(s)

  def consistentSetModify(s: S, a: A): IsEq[S] =
    optional.set(a)(s) <==> optional.modify(_ => a)(s)

  def consistentModifyModifyId(s: S, f: A => A): IsEq[S] =
    optional.modify(f)(s) <==> optional.modifyF(a => id.point(f(a)))(s)

  @OnlyInCats
  def consistentGetOptionModifyId(s: S): IsEq[Option[A]] =
    optional.getOption(s) <==> optional.modifyF[Const[First[A], ?]](a => Const(Some(a).first))(s).getConst.unwrap

  @OnlyInScalaz
  def consistentGetOptionModifyId(s: S): IsEq[Option[A]] =
    optional.getOption(s) <==> optional.modifyF[Const[Option[A] @@ First, ?]](a => Const(Some(a).first))(s).getConst.unwrap
}