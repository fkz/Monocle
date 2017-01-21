package monocle.law.discipline.function

import monocle.function.Each._
import monocle.function._
import monocle.law.discipline.TraversalTests
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

import monocle.catssupport.Implicits._


object EachTests extends Laws {

  def apply[S: Equal : Arbitrary, A: Equal : Arbitrary](implicit evEach: Each[S, A], arbAA: Arbitrary[A => A]): RuleSet =
    new SimpleRuleSet("Each", TraversalTests(each[S, A]).props: _*)

}