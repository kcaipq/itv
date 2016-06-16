package kaka

import scala.annotation.tailrec

/**
  * Created by kcai on 16/06/2016.
  */


/**
  * Price rule case class
  *
  * @param offerQuant the quantity in the price offer
  * @param offerTotal the total price for the quantity
  */
case class PricingRule(offerQuant: Int, offerTotal: BigDecimal)

/**
  * Containers to hold SKUS
  * @param skus a List of selected SKUS
  */
case class Basket(skus: List[SKU] = Nil)

/**
  * SKU
  * @param item SKU name
  * @param price SKU unit price
  * @param offers available price rules at the moment
  */
case class SKU(item: String, price: BigDecimal, offers: Option[Set[PricingRule]])

/**
  * Simple checkout service object
  */
object CheckoutService {

  /**
    * Apply the best available offer to the SKU list for a particular SKU to calculate the total price.
    * When multiple pricing rules apply to a SKU the cheapest stretegy is used.
    *
    * @param price SKU unit price
    * @param quant quantity of a SKU in basket
    * @param offers all available price rules
    * @return total price with offer applied if available for a SKU list
    */
  def bestTotal(price: BigDecimal, quant: Int, offers: Option[Set[PricingRule]]) = {
    val stdPrice = price * quant
    offers match {
      case Some(offs) =>
        // apply the min total for the item
        offs.map(x => quant % x.offerQuant * price + (quant / x.offerQuant) * x.offerTotal).min

      case None => stdPrice
    }
  }

  /**
    *Calculate the total price for all SKUS in the basket
    *
    * @param basket the basket contains selected SKUs
    * @return total price
    */
  def checkout(basket: Basket): BigDecimal = {
    val grouped = basket.skus.groupBy(w => w)

    @tailrec
    def sum(savedSkus: List[SKU], total: BigDecimal): BigDecimal = {
      savedSkus match {
        case Nil => total
        case x :: tail =>
          val skus = grouped.getOrElse(x, Nil)
          sum(tail, total + bestTotal(x.price, skus.size, x.offers))
      }
    }
    sum(grouped.keys.toList, 0.00)
  }
}