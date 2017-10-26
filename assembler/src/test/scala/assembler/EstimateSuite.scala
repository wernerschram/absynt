package assembler

import org.scalatest.{Matchers, WordSpec}

class EstimateSuite extends WordSpec with Matchers {
  "an Estimate" when {
    "created" should {
      "return a Bounded estimate for different values for minimum and maximum" in {
        val value: Estimate[Int] = Estimate(5, 10)
        value shouldBe a[Bounded[_]]
      }

      "return a Actual value for equal values for minimum and maximum" in {
        val value: Estimate[Int] = Estimate(7, 7)
        value shouldBe a[Actual[_]]
      }
    }

    "added" should {
      "return a Bounded estimate when an Actual value is added to a Bounded estimate" in {
        val bounded: Bounded[Int] = Bounded(1, 2)
        val actual: Actual[Int] = Actual(5)
        val result = Estimate.reduceInner[Int](_ + _)(bounded, actual)
        result shouldBe a[Bounded[_]]
      }

      "return the sum of minima and maxima for a set of bounded values" in {
        val estimates: List[Estimate[Int]] = List(Bounded(1, 1), Bounded(2, 2), Bounded(1, 3))
        val result = estimates.estimateSum
        result shouldBe Bounded(4, 6)
      }

      "return the sum of values for a set of Actual values" in {
        val estimates: List[Estimate[Int]] = List(Actual(5), Actual(2), Actual(3))
        val result = estimates.estimateSum
        result shouldBe Actual(10)
      }

      "return the sum of actual values added to the Bounded minima and maxima for a set of mixed Actual and Bounded values" in {
        val estimates: List[Estimate[Int]] = List(Actual(5), Bounded(2, 5), Actual(3), Bounded(4, 7))
        val result = estimates.estimateSum
        result shouldBe Bounded(14, 20)
      }

      "return Unknown for a set of values that contains an Unknown" in {
        val estimates: List[Estimate[Int]] = List(Actual(5), Unknown, Actual(3), Bounded(1, 4))
        val result = estimates.estimateSum
        result shouldBe Unknown
      }

      "return 0 for an empty list" in {
        val estimates: List[Estimate[Int]] = List.empty[Estimate[Int]]
        val result = estimates.estimateSum
        result shouldBe Actual(0)
      }
    }
  }
}
