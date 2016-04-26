package com.wix.pay.tranzila

trait TranzilaAuthorizationParser {
  def parse(authorizationKey: String): TranzilaAuthorization
  def stringify(authorization: TranzilaAuthorization): String
}
