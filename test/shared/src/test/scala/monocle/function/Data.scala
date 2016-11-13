package monocle.function

import monocle._
import org.scalacheck.{Cogen, Arbitrary}

import cats.data.NonEmptyList
import cats.Eq
import cats.syntax.applicative._

case class MMap[K, V](map: Map[K, V])

object MMap {
  def toMap[K, V]: Iso[MMap[K, V], Map[K, V]] =
    Iso[MMap[K, V], Map[K, V]](_.map)(MMap(_))

  implicit def mmapEq[K, V]: Eq[MMap[K, V]] = Eq.fromUniversalEquals
  implicit def mmapArb[K: Arbitrary, V: Arbitrary]: Arbitrary[MMap[K, V]] =
    Arbitrary(Arbitrary.arbitrary[Map[K, V]].map(MMap(_)))
}

case class CNel(head: Char, tail: List[Char])

object CNel extends TestInstances {
  val toNel: Iso[CNel, NonEmptyList[Char]] =
    Iso[CNel, NonEmptyList[Char]](c => NonEmptyList(c.head, c.tail))(n => CNel(n.head, n.tail.toList))

  implicit val cNelEq: Eq[CNel] = Eq.fromUniversalEquals
  implicit val cNelArb: Arbitrary[CNel] = Arbitrary (for {
    a1 <- Arbitrary.arbitrary[Char]
    a2 <- Arbitrary.arbitrary[List[Char]]
  } yield CNel(a1, a2))
}

case class CList(list: List[Char])

object CList {
  val toList: Iso[CList, List[Char]] = Iso[CList, List[Char]](_.list)(CList(_))

  implicit val clistEq: Eq[CList] = Eq.fromUniversalEquals
  implicit val clistArb: Arbitrary[CList] = Arbitrary(Arbitrary.arbitrary[List[Char]].map(CList(_)))
  implicit val clistCoGen: Cogen[CList] = Cogen.cogenList[Char].contramap[CList](_.list)
}

case class Raw(b: Boolean, c: Char, i: Int, l: Long, f: Float, d: Double)

object Raw extends TestInstances {
  val toTuple: Iso[Raw, (Boolean, Char, Int, Long, Float, Double)] =
    Iso((r: Raw) => (r.b, r.c, r.i, r.l, r.f, r.d))((Raw.apply _)tupled)

  implicit val rawEq: Eq[Raw] = Eq.fromUniversalEquals
  implicit val rawArb: Arbitrary[Raw] = Arbitrary(for {
    a1 <- Arbitrary.arbitrary[Boolean]
    a2 <- Arbitrary.arbitrary[Char]
    a3 <- Arbitrary.arbitrary[Int]
    a4 <- Arbitrary.arbitrary[Long]
    a5 <- Arbitrary.arbitrary[Float]
    a6 <- Arbitrary.arbitrary[Double]
  } yield Raw.apply(a1, a2, a3, a4, a5, a6))
}