package com.wix.pay.tranzila.model

object StatusCodes {
  /** Fraud. */
  val fraud = "903"
}

object IsTranzilaRejectedStatusCode {
  private val rejectedStatusCodes = Set(
    StatusCodes.fraud
  )

  def unapply(statusCode: String): Option[String] = {
    if (rejectedStatusCodes.contains(statusCode)) Some(statusCode) else None
  }
}