import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Prop, Properties}

object VariableLengthQuantitySpecification extends Properties("VariableLengthQuantityTest") {
  private def nonEmptyListOfInts = Gen.nonEmptyListOf(arbitrary[Int])

  property("encodeDecodeSingle") = Prop.forAll(nonEmptyListOfInts) { nums =>
    Right(nums) == VariableLengthQuantity.decode(VariableLengthQuantity.encode(nums))
  }

  property("encodeDecodeMultiple") = Prop.forAll(Gen.nonEmptyListOf(nonEmptyListOfInts)) { nums =>
    Right(nums.flatten) == VariableLengthQuantity.decode(nums.flatMap(VariableLengthQuantity.encode))
  }
}
