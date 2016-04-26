package com.wix.pay.tranzila

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonTranzilaAuthorizationParser() extends TranzilaAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): TranzilaAuthorization = {
    Serialization.read[TranzilaAuthorization](authorizationKey)
  }

  override def stringify(authorization: TranzilaAuthorization): String = {
    Serialization.write(authorization)
  }
}
