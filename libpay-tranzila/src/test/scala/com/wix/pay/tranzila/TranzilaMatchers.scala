package com.wix.pay.tranzila

import org.specs2.matcher.{AlwaysMatcher, Matcher, Matchers}

trait TranzilaMatchers extends Matchers {
  def authorizationParser: TranzilaAuthorizationParser

  def beMerchant(username: Matcher[String] = AlwaysMatcher()): Matcher[TranzilaMerchant] = {
    username ^^ { (_: TranzilaMerchant).username aka "username" }
  }

  def beAuthorization(index: Matcher[String] = AlwaysMatcher(),
                      confirmationCode: Matcher[String] = AlwaysMatcher()): Matcher[TranzilaAuthorization] = {
    index ^^ { (_: TranzilaAuthorization).index aka "currency" } and
      confirmationCode ^^ { (_: TranzilaAuthorization).confirmationCode aka "confirmation code" }
  }

  def beAuthorizationKey(authorization: Matcher[TranzilaAuthorization]): Matcher[String] = {
    authorization ^^ { authorizationParser.parse(_: String) aka "parsed authorization"}
  }

}

object TranzilaMatchers extends TranzilaMatchers {
  override val authorizationParser = new JsonTranzilaAuthorizationParser()
}