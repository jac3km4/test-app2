
trait PricingRule {
  def calculatePrice(quantity: Int): Int
}

object PricingRule {
  def apply(fn: Int => Int): PricingRule = fn(_)
  def constant(price: Int): PricingRule = _ * price

  // sorts packs by the quantity (the largest packs are of highest priority)
  // and applies them recursively
  def countByPacks(packPrices: List[PackPrice], defaultUnitPrice: Int): PricingRule =
    new PricingRule {
      override def calculatePrice(quantity: Int): Int =
        calc(packPrices.sortBy(_.quantity)(Ordering[Int].reverse), quantity)(defaultUnitPrice)

      def calc(remainingPackPrices: List[PackPrice], remainingQuantity: Int)
              (defaultUnitPrice: Int): Int =
        remainingPackPrices match {
          case head :: tail =>
            val packsCount = remainingQuantity / head.quantity
            val totalPrice = packsCount * head.price
            totalPrice + calc(tail, remainingQuantity - packsCount * head.quantity)(defaultUnitPrice)
          case _ => defaultUnitPrice * remainingQuantity
        }
    }

  final case class PackPrice(quantity: Int, price: Int)
}

object Checkout {
  type ProductId = Int

  def checkout(rules: Map[ProductId, PricingRule])
              (products: List[ProductId]): Either[String, Int] =
    products
      .groupBy(identity)
      .foldLeft[Either[String, Int]](Right(0)) { case (acc, (productId, entries)) =>
      rules.get(productId) match {
        case Some(rule) => acc.map(_ + rule.calculatePrice(entries.length))
        case None => Left(s"No pricing rule specified for $productId")
      }
    }
}
