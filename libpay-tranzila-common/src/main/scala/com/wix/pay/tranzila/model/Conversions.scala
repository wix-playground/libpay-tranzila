package com.wix.pay.tranzila.model

import java.text.DecimalFormat

object Conversions {
  private val amountFormat = new DecimalFormat("0.00")

  def toTranzilaAmount(amount: Double): String = {
    amountFormat.format(amount)
  }

  def toTranzilaCurrency(currency: String): String = {
    currency match {
      case "ILS" => Currencies.ILS
      case "USD" => Currencies.USD
      case "GBP" => Currencies.GBP
      case "HKD" => Currencies.HKD
      case "JPY" => Currencies.JPY
      case "EUR" => Currencies.EUR
      case _ => throw new IllegalArgumentException(s"Tranzila doesn't support $currency currency")
    }
  }

  def toTranzilaYearMonth(year: Int, month: Int): String = {
    f"$month%02d${year % 100}%02d"
  }
}
