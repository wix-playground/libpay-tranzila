package com.wix.pay.tranzila

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonTranzilaMerchantParser() extends TranzilaMerchantParser {
  private implicit val formats = DefaultFormats

  override def parse(merchantKey: String): TranzilaMerchant = {
    Serialization.read[TranzilaMerchant](merchantKey)
  }

  override def stringify(merchant: TranzilaMerchant): String = {
    Serialization.write(merchant)
  }
}
