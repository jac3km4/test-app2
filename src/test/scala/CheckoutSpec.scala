import PricingRule.PackPrice
import org.scalatest.{FlatSpec, Matchers}

class CheckoutSpec extends FlatSpec with Matchers {
  val AUnitPrice = 50
  val BUnitPrice = 30
  val CUnitPrice = 20
  val DUnitPrice = 15

  val AProduct = 1
  val BProduct = 2
  val CProduct = 3
  val DProduct = 4

  val rules = Map(
    AProduct -> PricingRule.countByPacks(List(PackPrice(3, 130)), AUnitPrice),
    BProduct -> PricingRule.countByPacks(List(PackPrice(2, 45)), BUnitPrice),
    CProduct -> PricingRule.constant(CUnitPrice),
    DProduct -> PricingRule.constant(DUnitPrice)
  )

  val alternative = Map(
    AProduct -> PricingRule.countByPacks(List(PackPrice(3, 130), PackPrice(4, 150)), AUnitPrice),
    BProduct -> PricingRule.countByPacks(List(PackPrice(2, 45), PackPrice(3, 50)), BUnitPrice),
    CProduct -> PricingRule.constant(CUnitPrice),
    DProduct -> PricingRule.constant(DUnitPrice)
  )

  it should "should properly calculate prices for products of constant pricing rules" in {
    val products = List(CProduct, DProduct, CProduct, CProduct, DProduct)
    Checkout.checkout(rules)(products) shouldBe Right(3 * CUnitPrice + 2 * DUnitPrice)
  }

  it should "should return Left when provided an unknown product" in {
    val products = List(CProduct, CProduct, CProduct, DProduct, DProduct, 5)
    Checkout.checkout(rules)(products).isLeft shouldBe true
  }

  it should "should properly calculate prices for products with special pack prices" in {
    val products = List(AProduct, BProduct, AProduct, CProduct, AProduct, DProduct)
    Checkout.checkout(rules)(products) shouldBe Right(130 + BUnitPrice + CUnitPrice + DUnitPrice)
  }

  it should "properly calculate prices of product quantities not divisible by pack quantities" in {
    val products = List(AProduct, BProduct, AProduct, CProduct, AProduct, DProduct, AProduct)
    Checkout.checkout(rules)(products) shouldBe Right(130 + BUnitPrice + CUnitPrice + DUnitPrice + AUnitPrice)
  }

  it should "prioritize the largest pack options when performing the calculation" in {
    val products = List(AProduct, AProduct, BProduct, BProduct, AProduct, BProduct, AProduct)
    Checkout.checkout(alternative)(products) shouldBe Right(200)
  }

}
