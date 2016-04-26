package com.wix.pay.tranzila


import com.wix.pay.tranzila.TranzilaMatchers._
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class JsonTranzilaAuthorizationParserTest extends SpecWithJUnit {
  trait Ctx extends Scope {
    val authorizationParser: TranzilaAuthorizationParser = new JsonTranzilaAuthorizationParser
  }

  "stringify and then parse" should {
    "yield an authorization similar to the original one" in new Ctx {
      val someAuthorization = TranzilaAuthorization(
        index = "some index",
        confirmationCode = "some confirmation code"
      )

      val authorizationKey = authorizationParser.stringify(someAuthorization)
      authorizationParser.parse(authorizationKey) must beAuthorization(
        index = ===(someAuthorization.index),
        confirmationCode = ===(someAuthorization.confirmationCode)
      )
    }
  }
}
