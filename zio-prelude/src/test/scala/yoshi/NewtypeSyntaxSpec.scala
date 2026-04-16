package yoshi

import yoshi.defaults.*
import yoshi.prelude.*
import zio.prelude.Assertion
import zio.prelude.Newtype
import zio.test.*

object WrappedString extends Newtype[String]
type WrappedString = WrappedString.Type

object PositiveInt extends Newtype[Int] {
  override inline def assertion: Assertion[Int] = Assertion.greaterThan(0)
}
type PositiveInt = PositiveInt.Type

object BoundedInt extends Newtype[Int] {
  override inline def assertion: Assertion[Int] =
    Assertion.greaterThanOrEqualTo(0) && Assertion.lessThanOrEqualTo(100)
}
type BoundedInt = BoundedInt.Type

object NewtypeSyntaxSpec extends ZIOSpecDefault {

  override def spec = suiteAll("Validation.newtype") {
    test("succeeds for Newtype with default assertion") {
      val v = Validation.newtype(WrappedString)((value, msg) => s"$value: $msg")
      assertTrue(v.run("anything") == Right(WrappedString("anything")))
    }
    test("succeeds when custom assertion passes") {
      val v = Validation.newtype(PositiveInt)((value, _) => s"$value must be positive")
      assertTrue(v.run(5) == Right(PositiveInt(5)))
    }
    test("fails with caller-defined violation when assertion fails") {
      val v = Validation.newtype(PositiveInt)((value, _) => s"$value must be positive")
      assertTrue(v.run(0) == Left(Violations.of("0 must be positive")))
    }
    test("fails with compound assertion") {
      val v                = Validation.newtype(BoundedInt)((value, msg) => s"$value: $msg")
      val Left(violations) = v.run(200): @unchecked
      assertTrue(violations.values.nonEmpty)
    }
    test("composes with >> for String to newtype") {
      val v: Validation[Violation, String, PositiveInt] =
        Validations.parseInt >> Validation.newtype(PositiveInt)((value, _) => Violation.NonPositive(value))
      assertTrue(v.run("42") == Right(PositiveInt(42)))
    }
  }
}
