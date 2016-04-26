package com.wix.pay.tranzila


import com.wix.pay.tranzila.TranzilaMatchers._
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class JsonTranzilaMerchantParserTest extends SpecWithJUnit {
  trait Ctx extends Scope {
    val merchantParser: TranzilaMerchantParser = new JsonTranzilaMerchantParser
  }


  "stringify and then parse" should {
    "yield a merchant similar to the original one" in new Ctx {
      val someMerchant = TranzilaMerchant(
        username = "some username"
      )

      val merchantKey = merchantParser.stringify(someMerchant)
      merchantParser.parse(merchantKey) must beMerchant(
        username = ===(someMerchant.username)
      )
    }
  }
}
