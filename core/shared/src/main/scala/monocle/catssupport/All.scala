package monocle.catssupport

import cats.data.{NonEmptyList, State}
import cats.functor.Profunctor
import cats.kernel.Semigroup
import cats.{Applicative, Eq, Functor, Monoid, Unapply}

import scala.util.control.NonFatal

 class Disjunction(val unwrap: Boolean) extends AnyVal

 object Disjunction {
  implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
    def empty: Disjunction = new Disjunction(false)

    def combine(x: Disjunction, y: Disjunction): Disjunction = new Disjunction(x.unwrap || y.unwrap)
  }
}

 class Conjunction(val unwrap: Boolean) extends AnyVal

 object Conjunction {
  implicit val ConjunctionMonoid: Monoid[Conjunction] = new Monoid[Conjunction] {
    def empty: Conjunction = new Conjunction(true)

    def combine(x: Conjunction, y: Conjunction): Conjunction = new Conjunction(x.unwrap && y.unwrap)
  }
}

 class First[A](val unwrap: Option[A]) extends AnyVal

 object First {
  implicit def optionMonoid[A]: Monoid[First[A]] = new Monoid[First[A]] {
    def empty: First[A] = new First(None)

    def combine(x: First[A], y: First[A]): First[A] = x.unwrap match {
      case None => y
      case Some(_) => x
    }
  }
}

object Implicits {
  implicit class BooleanExtension(val b: Boolean) extends AnyVal {
    def conjunction: Conjunction = new Conjunction(b)

    def disjunction: Disjunction = new Disjunction(b)
  }

  implicit class OptionExtensions[A](val o: Option[A]) extends AnyVal {
    def first: First[A] = new First(o)
    def toOption: Option[A] = o
    def toMaybe: Option[A] = o
  }

  type \/[+A, +B] = Either[A, B]

  object \/ {
    def left[A](a: A): Left[A, Nothing] = Left(a)
    def right[B](b: B): Right[Nothing, B] = Right(b)
    def fromTryCatchNonFatal[A](f: => A): Either[Throwable, A] = {
      try {
        Right(f)
      }
      catch {
        case NonFatal(t) =>
          Left(t)
      }
    }

    def fromEither[A, B](x: Either[A, B]): Either[A, B] = x
  }

  implicit class EitherExtensions[A, B](val either: Either[A, B]) extends AnyVal {
    def leftMap[C](f: A => C): Either[C, B] = either.left.map(f)
    def bimap[C, D](f1: A => C, f2: B => D): Either[C, D] =
      either match {
        case Left(a) => Left(f1(a))
        case Right(b) => Right(f2(b))
      }

    def toEither: Either[A, B] = either
  }

  class Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H](proof: A =:= B) {
    def apply(a: A): B = proof(a)
    def subst[F[_ >: L <: H]](p: F[A]): F[B] = p.asInstanceOf[F[B]]

    //def compose[L2 <: L, H2 >: H, C >: L2 <: H2](that: Leibniz[L2, H2, C, A]): Leibniz[L2, H2, C, B] =
    //  Leibniz.trans[L2, H2, C, A, B](this.proof.(that.proof))
    //def andThen[L2 <: L, H2 >: H, C >: L2 <: H2](that: Leibniz[L2, H2, B, C]): Leibniz[L2, H2, A, C] =
    //  Leibniz.trans[L2, H2, A, B, C](that, this)

    def onF[X](fa: X => A): X => B = subst[X => ?](fa)
    //def onCov[FA](fa: FA)(implicit U: Unapply.Aux1[Functor, FA, A]): U.M[B] =
    //  subst(U(fa))
    //def onContra[FA](fa: FA)(implicit U: Unapply.AuxA[Contravariant, FA, A]): U.M[B] =
    //  subst(U(fa))
  }

  object Leibniz {
    implicit def leibniz[A, B](implicit p: A =:= B): Leibniz[Nothing, Any, A, B] = new Leibniz[Nothing, Any, A, B](p)
  }

  type ===[A, B] = Leibniz[Nothing, Any, A, B]

  class <~<[A, B](proof: A <:< B) {
    def apply(a: A): B = proof(a)
    def subst[F[-_]](p: F[B]): F[A] = p.asInstanceOf[F[A]]
  }

  object <~< {
    implicit def gen_<[A, B](implicit p: A <:< B): A <~< B = new <~<(p)
  }

  implicit class MonoidExt[B](val m: Monoid[B]) extends AnyVal {
    def zero = m.empty
    def append(a: B, b: B): B = m.combine(a, b)
  }

  implicit class ApplicativeExt[F[_]](val m: Applicative[F]) extends AnyVal {
    def point[A](t: A): F[A] = m.pure(t)

    def apply2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] = m.map2(a, b)(f)
    def apply3[A, B, C, D](a: F[A], b: F[B], c: F[C])(f: (A, B, C) => D): F[D] = m.map3(a, b, c)(f)
  }

  implicit class ProfunctorExt[F[_, _]](val p: Profunctor[F]) extends AnyVal {
    def mapfst[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
      p.lmap(fab)(f)
    def mapsnd[A,B,C](fab: F[A, B])(f: B => C): F[A, C] =
      p.rmap(fab)(f)
  }

  class Validation[+A, +B](val e: Either[A, B]) extends AnyVal

  object Validation {
    implicit def applicative[Err](implicit ev: Semigroup[Err]): Applicative[Validation[Err,?]] = new Applicative[Validation[Err, ?]] {
      override def pure[A](x: A): Validation[Err, A] = new Validation(Right(x))

      override def ap[A, B](ff: Validation[Err, (A) => B])(fa: Validation[Err, A]): Validation[Err, B] = {
        (ff.e, fa.e) match {
          case (Left(err1), Left(err2)) => new Validation(Left(ev.combine(err1, err2)))
          case (Left(err), Right(_)) => ff.asInstanceOf[Validation[Err, B]]
          case (Right(_), Left(err)) => fa.asInstanceOf[Validation[Err, B]]
          case (Right(f), Right(v)) => new Validation(Right(f(v)))
        }
      }
    }

    def failure[B](b: B): Validation[B, Nothing] = new Validation(Left(b))
  }

  sealed trait Either3[+A, +B, +C]
  case class Left3[+A](value: A) extends  Either3[A, Nothing, Nothing]
  case class Middle3[+B](value: B) extends Either3[Nothing, B, Nothing]
  case class Right3[+C](value: C) extends Either3[Nothing, Nothing, C]

  object Either3 {
    def left3[A](a: A) = Left3(a)
    def middle3[A](a: A) = Middle3(a)
    def right3[A](a: A) = Right3(a)

    implicit def eqForEither[A: Eq, B: Eq, C: Eq]: Eq[Either3[A, B, C]] = Eq.instance((a1, a2) =>
      (a1, a2) match {
        case (Left3(a), Left3(b)) => Eq[A].eqv(a, b)
        case (Middle3(a), Middle3(b)) => Eq[B].eqv(a, b)
        case (Right3(a), Right3(b)) => Eq[C].eqv(a, b)
        case _ => false
      }
    )
  }

  type Equal[A] = Eq[A]

  implicit class EqualCompanionObjectExtensions(val eq: Eq.type) extends AnyVal {
    def equalA[A]: Eq[A] = Eq.fromUniversalEquals[A]
    def equal[A](f: (A, A) => Boolean): Eq[A] = Eq.instance(f)
  }

  implicit class EqualExtensions[A](val eq: Eq[A]) extends AnyVal {
    def contramap[B](fun: B => A): Eq[B] = eq.on(fun)
    def equal(a: A, b: A): Boolean = eq.eqv(a, b)
  }

  implicit class NonEmptyListCompanionObjectExtensions(val e: NonEmptyList.type) extends AnyVal {
    def nel[A](head: A, tail: List[A]): NonEmptyList[A] = {
      NonEmptyList(head, tail)
    }
  }

  implicit class NonEmptyListExtensions[A](val nel: NonEmptyList[A]) extends AnyVal {
    def zipWithIndex: NonEmptyList[(A, Int)] = NonEmptyList.fromListUnsafe(nel.toList.zipWithIndex)
    def reverse: NonEmptyList[A] = NonEmptyList.fromListUnsafe(nel.toList.reverse)
    def init: List[A] = nel.toList.init
    def last: A = nel.toList.last
    def list: List[A] = nel.toList
  }

  implicit class StateCompanionObjectExtensions(val s: State.type) extends AnyVal {
    def state[S, A](a: A): State[S, A] = s.pure(a)
  }

  implicit class StateExtensions[S, A](val s: State[S, A]) extends AnyVal {
    def runZero(implicit z: Monoid[S]): (S, A) = s.runEmpty.value
  }

  type IList[A] = List[A]
  type Maybe[A] = Option[A]
  val Just = Some
  def Nothing[A] = None
  type ISet[A] = Set[A]

  object Maybe {
    def empty[A]: Option[A] = None
  }

  object IList {
    def fromList[A](x: List[A]): List[A] = x
    def empty[A](): List[A] = Nil
  }

  object INil {
    def unapply[A](a: Nil.type): Boolean = true
  }

  object ICons {
    def unapply[A](a: ::[A]): Option[(A, List[A])] = ::.unapply(a)
  }

  implicit class ListExtensions[A](val list: List[A]) extends AnyVal {
    def foldl[B](init: B)(f: B => A => B): B = list.foldLeft(init)((b, a) => f(b)(a))
  }

  type ==>>[A, B] = Map[A, B]
}
