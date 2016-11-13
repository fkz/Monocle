package monocle

import cats._
import cats.arrow.{Category, Choice, Compose}
import cats.instances.boolean._
import cats.instances.list._
import cats.instances.string._
import cats.instances.int._
import catssupport.Implicits._

class FoldSpec extends MonocleSuite {

  val iListFold = Fold.fromFoldable[List, Int]

  test("foldMap") {
    iListFold.foldMap(_.toString)(List(1,2,3,4,5)) shouldEqual "12345"
  }

  test("headMaybe") {
    iListFold.headOption(List(1,2,3,4,5)) shouldEqual Some(1)
    iListFold.headOption(Nil)             shouldEqual None
  }

  test("exist") {
    iListFold.exist(_ % 2 == 0)(List(1,2,3)) shouldEqual true
    iListFold.exist(_ == 7)(List(1,2,3))     shouldEqual false
  }

  test("all") {
    iListFold.all(_ % 2 == 0)(List(1,2,3)) shouldEqual false
    iListFold.all(_ <= 7)(List(1,2,3))     shouldEqual true
  }

  test("length") {
    iListFold.length(List(1,2,3,4,5)) shouldEqual 5
    iListFold.length(Nil)             shouldEqual 0
  }

  def nestedIListFold[A] = new Fold[List[List[A]], List[A]]{
    def foldMap[M: Monoid](f: (List[A]) => M)(s: List[List[A]]): M =
      s.foldRight(Monoid[M].zero)((l, acc) => Monoid[M].append(f(l), acc))
  }

  // test implicit resolution of type classes

  test("Fold has a Compose instance") {
    Compose[Fold].compose(iListFold, nestedIListFold[Int]).fold(List(List(1,2,3), List(4,5), List(6))) shouldEqual 21
  }

  test("Fold has a Category instance") {
    Category[Fold].id[Int].fold(3) shouldEqual 3
  }

  test("Fold has a Choice instance") {
    Choice[Fold].choice(iListFold, Choice[Fold].id[Int]).fold(\/.left(List(1,2,3))) shouldEqual 6
  }

}