package com.wix.pay.tranzila

trait TranzilaMerchantParser {
  def parse(merchantKey: String): TranzilaMerchant
  def stringify(merchant: TranzilaMerchant): String
}
