package monocle

import org.scalacheck.Arbitrary._
import org.scalacheck.rng.Seed
import org.scalacheck.{Cogen, Arbitrary, Gen}
import org.scalactic.Equality

//import scalaz.Tree.Node
//import scalaz.\&/.{Both, That, This}
import cats._
import cats.instances.all._
import cats.syntax.traverse._
import catssupport.Implicits._
import cats.data.OneAnd
import cats.data.NonEmptyList

trait TestInstances {

  implicit def equality[A](implicit A: Eq[A]): Equality[A] =
    new Equality[A]{
      override def areEqual(a: A, b: Any): Boolean =
        A.eqv(a, b.asInstanceOf[A])
    }

  implicit val genApplicative: Applicative[Gen] = new Applicative[Gen] {
    def ap[A, B](fa: => Gen[A])(f: => Gen[A => B]): Gen[B] = fa.flatMap(a => f.map(_(a)))
    def point[A](a: => A): Gen[A] = Gen.const(a)

    override def pure[A](x: A): Gen[A] = point(x)
    override def ap[A, B](ff: Gen[(A) => B])(fa: Gen[A]): Gen[B] = ap(fa)(ff)
  }

  // Equal instances
  implicit val booleanEqual    = Eq[Boolean]
  implicit val byteEqual       = Eq[Byte]
  implicit val shortEqual      = Eq[Short]
  implicit val charEqual       = Eq[Char]
  implicit val intEqual        = Eq[Int]
  implicit val longEqual       = Eq[Long]
  implicit val floatEqual      = Eq[Float]
  implicit val doubleEqual     = Eq[Double]
  implicit val stringEqual     = Eq[String]
  implicit val unitEqual       = Eq[Unit]
  implicit val bigIntEqual     = Eq[BigInt]
  implicit val bigDecimalEqual = Eq[BigDecimal]

  implicit def optEq[A: Eq] = Eq[Option[A]]
  implicit def someEq[A: Eq] = Eq.by[Some[A], A](_.value)
  implicit def eitherEq[A: Eq, B: Eq] = cats.instances.either.catsStdEqForEither[A, B]
  implicit def listEq[A: Eq] = cats.instances.list.catsKernelStdEqForList[A]
  implicit def vectorEq[A: Eq] = cats.instances.vector.catsKernelStdEqForVector[A]
  implicit def streamEq[A: Eq] = cats.instances.stream.catsKernelStdEqForStream[A]
  implicit def setEq[A: Order] = Eq.instance[Set[A]]((s1, s2) => {
    val s1l: List[A] = s1.toList.sorted(Order[A].toOrdering)
    val s2l: List[A] = s2.toList.sorted(Order[A].toOrdering)
    Eq[List[A]].eqv(s1l, s2l)
  })
  implicit def mapEq[K/*: Order*/, V: Eq] = cats.instances.map.catsKernelStdEqForMap[K, V]

  implicit def tuple2Eq[A1: Eq, A2: Eq] = cats.instances.tuple.catsKernelStdEqForTuple2[A1, A2]
  implicit def tuple3Eq[A1: Eq, A2: Eq, A3: Eq] = cats.instances.tuple.catsKernelStdEqForTuple3[A1, A2, A3]
  implicit def tuple4Eq[A1: Eq, A2: Eq, A3: Eq, A4: Eq] = cats.instances.tuple.catsKernelStdEqForTuple4[A1, A2, A3, A4]
  implicit def tuple5Eq[A1: Eq, A2: Eq, A3: Eq, A4: Eq, A5: Eq] = cats.instances.tuple.catsKernelStdEqForTuple5[A1, A2, A3, A4, A5]
  implicit def tuple6Eq[A1: Eq, A2: Eq, A3: Eq, A4: Eq, A5: Eq, A6: Eq] = cats.instances.tuple.catsKernelStdEqForTuple6[A1, A2, A3, A4, A5, A6]

  //implicit def optionCofreeEq[A](implicit A: Equal[A]): Equal[Cofree[Option, A]] =
  //  Equal.equal { (a, b) =>  A.equal(a.head, b.head) && a.tail === b.tail }

  //implicit def streamCofreeEq[A](implicit A: Equal[A]): Equal[Cofree[Stream, A]] =
  //  Equal.equal { (a, b) =>  A.equal(a.head, b.head) && a.tail === b.tail }

  // Order instances

  implicit val intOrder = Order[Int]

  // Show instances

  implicit val intShow = Show[Int]

  //implicit def treeShow[A: Show] = new Show[Tree[A]] {
  //  override def shows(f: Tree[A]): String = f.drawTree
  //}

  implicit def streamShow[A: Show] = cats.instances.stream.catsStdShowForStream[A]

  // Arbitrary instances

//  implicit def treeArbitrary[A: Arbitrary]: Arbitrary[Tree[A]] =
//    Arbitrary {
//      def genPartition(sum: Int): Gen[List[Int]] =
//        if(sum <= 0) Gen.const(Nil)
//        else for {
//          n    <- Gen.choose(1, sum)
//          tail <- genPartition(sum - n)
//        } yield n :: tail
//
//      def sizedTree(size: Int): Gen[Tree[A]] =
//        for {
//          value      <- Arbitrary.arbitrary[A]
//          partitions <- genPartition(size - 1)
//          children   <- partitions.traverseU(sizedTree)
//        } yield Node[A](value, children.toStream)
//
//      Gen.sized(sz => sizedTree(sz))
//    }
//
//  implicit def treeCoGen[A: Cogen]: Cogen[Tree[A]] =
//    Cogen[Tree[A]]((seed: Seed, t: Tree[A]) => Cogen[(A, Stream[Tree[A]])].perturb(seed, (t.rootLabel, t.subForest)))

  implicit def streamCoGen[A: Cogen]: Cogen[Stream[A]] = Cogen[List[A]].contramap[Stream[A]](_.toList)

  implicit def optionArbitrary[A: Arbitrary]: Arbitrary[Option[A]] = Arbitrary(Gen.frequency(
    1 -> None,
    3 -> Arbitrary.arbitrary[A].map(Option(_))
  ))

  //implicit def maybeArbitrary[A: Arbitrary]: Arbitrary[Option[A]] = Arbitrary(Gen.frequency(
  //  1 -> Option.empty[A],
  //  3 -> Arbitrary.arbitrary[A].map(Option.just(_))
  //))

  //implicit def iListCoGen[A: Cogen]: Cogen[IList[A]] = Cogen[List[A]].contramap[IList[A]](_.toList)

  implicit def someArbitrary[A: Arbitrary]: Arbitrary[Some[A]] = Arbitrary(Arbitrary.arbitrary[A].map(Some(_)))

  //implicit def disjunctionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
  //  Arbitrary(arbitrary[Either[A, B]] map \/.fromEither)

  implicit def coGenDisjunction[E: Cogen, A: Cogen]: Cogen[E \/ A] =
    Cogen.cogenEither[E, A].contramap[E \/ A](_.toEither)

  //implicit def validationArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Validation[A, B]] =
  //  Arbitrary(arbitrary[A \/ B].map(_.validation))

  //implicit def coGenValidation[E: Cogen, A: Cogen]: Cogen[Validation[E, A]] =
  //  Cogen.cogenEither[E, A].contramap[Validation[E, A]](_.toEither)

  /*implicit def theseArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \&/ B] =
    Arbitrary(Gen.oneOf(
      arbitrary[A].map(This(_)),
      arbitrary[B].map(That(_)),
      for {
        a <- arbitrary[A]
        b <- arbitrary[B]
      } yield Both(a, b)))
*/
  implicit def oneAndArbitrary[T[_], A](implicit a: Arbitrary[A], ta: Arbitrary[T[A]]): Arbitrary[OneAnd[T, A]] = Arbitrary(for {
    head <- Arbitrary.arbitrary[A]
    tail <- Arbitrary.arbitrary[T[A]]
  } yield OneAnd(head, tail))

  implicit def oneAndCoGen[T[_], A](implicit a: Cogen[A], ta: Cogen[T[A]]): Cogen[OneAnd[T, A]] =
    Cogen[(A, T[A])].contramap[OneAnd[T, A]](o => (o.head, o.tail))

  implicit def vectorArbitrary[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(_.toVector))

  //implicit def iListArbitrary[A: Arbitrary]: Arbitrary[IList[A]] =
  //  Arbitrary(Arbitrary.arbitrary[List[A]].map(IList.fromList))

  implicit def mapArbitrary[K: Arbitrary, V: Arbitrary] =
    Arbitrary(Arbitrary.arbitrary[List[(K,V)]].map(_.toMap))

  //implicit def iMapArbitrary[K: Arbitrary: Order, V: Arbitrary] =
  //  Arbitrary(Arbitrary.arbitrary[List[(K,V)]].map(l => ==>>.fromList(l)(Order[K])))

  implicit def setArbitrary[A: Arbitrary]: Arbitrary[Set[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(_.toSet))

  //implicit def iSetArbitrary[A: Arbitrary: Order]: Arbitrary[ISet[A]] =
  //  Arbitrary(Arbitrary.arbitrary[List[A]].map(l => ISet.fromList(l)(Order[A])))

  implicit def nelArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(oneAndArbitrary[List,A].arbitrary.map( o => NonEmptyList(o.head, o.tail)))

  implicit def nelCoGen[A: Cogen]: Cogen[NonEmptyList[A]] =
    Cogen[(A, List[A])].contramap[NonEmptyList[A]](nel => (nel.head, nel.tail))

  implicit def either3Arbitrary[A: Arbitrary, B: Arbitrary, C: Arbitrary]: Arbitrary[Either3[A, B, C]] =
    Arbitrary(Gen.oneOf(
      Arbitrary.arbitrary[A].map(Either3.left3),
      Arbitrary.arbitrary[B].map(Either3.middle3),
      Arbitrary.arbitrary[C].map(Either3.right3)
    ))

  //implicit def optionCofreeArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Cofree[Option, A]] =
  //  Arbitrary(Arbitrary.arbitrary[OneAnd[List, A]].map( xs =>
  //    monocle.std.cofree.cofreeToStream.reverseGet(xs.copy(tail = xs.tail.toStream))
  //  ))

  //implicit def streamCofreeArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Cofree[Stream, A]] =
  // Arbitrary(Arbitrary.arbitrary[Tree[A]].map( monocle.std.cofree.cofreeToTree.reverseGet))

  //implicit def cogenOptionCofree[A](implicit A: Cogen[A]): Cogen[Cofree[Option, A]] =
  //  Cogen[Cofree[Option, A]]((seed: Seed, t: Cofree[Option, A]) => Cogen[(A, Option[Cofree[Option, A]])].perturb(seed, (t.head, t.tail)))

  //implicit def cogenStreamCofree[A](implicit A: Cogen[A]): Cogen[Cofree[Stream, A]] =
  //  Cogen[Cofree[Stream, A]]((seed: Seed, t: Cofree[Stream, A]) => Cogen[(A, Stream[Cofree[Stream, A]])].perturb(seed, (t.head, t.tail)))

}
