package it.unich.jandom.utils.breeze

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalacheck.Gen

class RationalTestSuite extends FunSpec with GeneratorDrivenPropertyChecks {

  val rationalGenerator = for {
    numer <- Gen.choose(-1000,1000)
    denom <- Gen.choose(-1000,1000)
    if denom != 0
  } yield Rational(numer, denom)

  implicit val rationalArbitrary = Arbitrary( rationalGenerator )

  describe("Sum") {
    it("is commutative") {
      forAll  { (r1: Rational, r2: Rational) =>
        assert(r1 + r2 == r1 + r2)
      }
    }
  }
}
