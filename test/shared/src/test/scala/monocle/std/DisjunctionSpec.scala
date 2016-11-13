package monocle.std

import monocle.MonocleSuite
import monocle.law.discipline.{IsoTests, PrismTests}

class DisjunctionSpec extends MonocleSuite {
  checkAll("disjunction left" , PrismTests(left[String, Int]))
  checkAll("disjunction right", PrismTests(right[String, Int]))

  //checkAll("disjunction to Validation", IsoTests(disjunctionToValidation[String, Int]))
  checkAll("disjunction to Either"    , IsoTests(disjunctionToEither[String, Int]))
}
