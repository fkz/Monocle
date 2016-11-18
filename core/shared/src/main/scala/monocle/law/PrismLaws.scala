package monocle.law

import monocle.Prism
import monocle.internal.IsEq
import cats.{catsInstancesForId => id}
import cats.instances.option._
import cats.syntax.option._
import cats.data.Const
import monocle.catssupport._
import monocle.catssupport.Implicits._

case class PrismLaws[S, A](prism: Prism[S, A]) {
  import IsEq.syntax

  def partialRoundTripOneWay(s: S): IsEq[S] =
    prism.getOrModify(s).fold(identity, prism.reverseGet) <==> s
  
  def roundTripOtherWay(a: A): IsEq[Option[A]] =
    prism.getOption(prism.reverseGet(a)) <==> Some(a)

  def modifyIdentity(s: S): IsEq[S] =
    prism.modify(identity)(s) <==> s

  def composeModify(s: S, f: A => A, g: A => A): IsEq[S] =
    prism.modify(g)(prism.modify(f)(s)) <==> prism.modify(g compose f)(s)

  def consistentSetModify(s: S, a: A): IsEq[S] =
    prism.set(a)(s) <==> prism.modify(_ => a)(s)

  def consistentModifyModifyId(s: S, f: A => A): IsEq[S] =
    prism.modify(f)(s) <==> prism.modifyF(a => id.point(f(a)))(s)

  def consistentGetOptionModifyId(s: S): IsEq[Option[A]] =
    prism.getOption(s) <==> prism.modifyF[Const[First[A], ?]](a => Const(Some(a).first))(s).getConst.unwrap
}