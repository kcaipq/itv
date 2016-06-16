package kaka

import kaka.CheckoutService._
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by kcai on 16/06/2016.
  */
class CheckoutServiceTest extends FlatSpec with Matchers {

  private val service = CheckoutService

  val offer1 = Set(PricingRule(3, 130))
  val offer2 = Set(PricingRule(5, 200))
  val offer3 = Set(PricingRule(2, 45))

  val A = SKU("A", 50, Some(offer1))
  val B = SKU("B", 30, Some(offer3))
  val C = SKU("C", 20, None)
  val D = SKU("D", 15, None)
  val E = SKU("E", 55, Some(offer1 ++ offer2))

  "Best Price For SKU without offer" should "be std total" in {
    service.bestTotal(50, 1, None) shouldBe(50)
  }

  "Best Price For SKU with one offer" should "have offer applied" in {
    service.bestTotal(50, 4, Some(offer1)) shouldBe(130 + 50)
  }

  "Best Price For SKU with two offers" should "have offers applied" in {
    service.bestTotal(50, 6, Some(offer1 ++ offer2)) shouldBe(250)
    service.bestTotal(50, 5, Some(offer1 ++ offer2)) shouldBe(200)
    service.bestTotal(50, 7, Some(offer1 ++ offer2)) shouldBe(300)
    service.bestTotal(50, 4, Some(offer1 ++ offer2)) shouldBe(180)
    service.bestTotal(50, 2, Some(offer1 ++ offer2)) shouldBe(100)
  }

  "checkout" should "pass" in {
    checkout(Basket(List())) shouldBe(0)
    checkout(Basket(List(A))) shouldBe(50)
    checkout(Basket(List(A,A))) shouldBe(100)
    checkout(Basket(List(A,A,A))) shouldBe(130)
    checkout(Basket(List(A,A,A,A))) shouldBe(180)

    checkout(Basket(List(B))) shouldBe(30)
    checkout(Basket(List(B,B))) shouldBe(45)
    checkout(Basket(List(B,B,B))) shouldBe(75)

    checkout(Basket(List(C))) shouldBe(20)
    checkout(Basket(List(C,C))) shouldBe(40)
    checkout(Basket(List(C,C,C))) shouldBe(60)

    checkout(Basket(List(D))) shouldBe(15)
    checkout(Basket(List(D,D))) shouldBe(30)
    checkout(Basket(List(D,D,D))) shouldBe(45)

    checkout(Basket(List(E))) shouldBe(55)
    checkout(Basket(List(E,E))) shouldBe(110)
    checkout(Basket(List(E,E,E))) shouldBe(130)
    checkout(Basket(List(E,E,E,E))) shouldBe(185)
    checkout(Basket(List(E,E,E,E,E))) shouldBe(200)
    checkout(Basket(List(E,E,E,E,E,E))) shouldBe(255)

    checkout(Basket(List(E,A,E,E,E,E))) shouldBe(250)
    checkout(Basket(List(A,B,C,D))) shouldBe(115)
    checkout(Basket(List(A,A,A,B,C))) shouldBe(180)
    checkout(Basket(List(A,B,B,C))) shouldBe(115)
    checkout(Basket(List(A,A,A,A,B,B,B,C,D))) shouldBe(290)
    checkout(Basket(List(A,A,A,A,A,B,B,B,C,D))) shouldBe(340)
  }

}
