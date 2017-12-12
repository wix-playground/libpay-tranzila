package com.wix.pay.tranzila.it


import scala.util.Try
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import com.google.api.client.http.HttpRequestFactory
import com.google.api.client.http.javanet.NetHttpTransport
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model._
import com.wix.pay.shva.model.StatusCodes
import com.wix.pay.tranzila.TranzilaMatchers._
import com.wix.pay.tranzila.model.Conversions._
import com.wix.pay.tranzila.model._
import com.wix.pay.tranzila.testkit.TranzilaDriver
import com.wix.pay.tranzila.{TranzilaAuthorization, TranzilaMerchant, _}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}


class TranzilaGatewayIT extends SpecWithJUnit {
  val tranzilaPort = 10009

  val requestFactory: HttpRequestFactory = new NetHttpTransport().createRequestFactory()
  val driver = new TranzilaDriver(port = tranzilaPort)

  val merchantParser = new JsonTranzilaMerchantParser()
  val authorizationParser = new JsonTranzilaAuthorizationParser()

  val someMerchant = TranzilaMerchant(username = "some username")
  val merchantKey: String = merchantParser.stringify(someMerchant)

  val somePayment = Payment(currencyAmount = CurrencyAmount("ILS", 33.3))
  val someAdditionalFields: CreditCardOptionalFields = CreditCardOptionalFields.withFields(
    csc = Some("123"),
    holderId = Some("some holder ID"))
  val someCreditCard = CreditCard(
    number = "4012888818888",
    expiration = YearMonth(2020, 12),
    additionalFields = Some(someAdditionalFields))
  val someCustomer = Customer(
    name = Some(Name(
      first = "some שם פרטי", // Hebrew characters to test for encoding issues
      last = "some last name")),
    email = Some("example@example.org"),
    phone = Some("+972545000000"))

  val someDeal = Deal(
    id = "some deal ID",
    title = Some("some deal title"),
    description = Some("some deal description"))

  val tranzila: PaymentGateway = new TranzilaGateway(
    requestFactory = requestFactory,
    endpointUrl = s"http://localhost:$tranzilaPort/",
    merchantParser = merchantParser,
    authorizationParser = authorizationParser)


  step {
    driver.start()
  }


  sequential


  trait Ctx extends Scope {
    driver.reset()
  }


  "authorize request via Tranzila gateway" should {
    "gracefully fail on invalid merchant key" in new Ctx {
      val someIndex = "some index"
      val someConfirmationCode = "some confirmation code"

      driver.aRequestFor(Map(
        Fields.supplier -> Some(someMerchant.username),
        Fields.sum -> Some(toTranzilaAmount(somePayment.currencyAmount.amount)),
        Fields.currency -> Some(toTranzilaCurrency(somePayment.currencyAmount.currency)),
        Fields.transactionMode -> Some(TransactionModes.VERIFY),
        Fields.ccNumber -> Some(someCreditCard.number),
        Fields.expDate -> Some(toTranzilaYearMonth(
          year = someCreditCard.expiration.year,
          month = someCreditCard.expiration.month)),
        Fields.cvv -> Some(someCreditCard.csc.get),
        Fields.holderId -> Some(someCreditCard.holderId.get),
        Fields.contact -> Some(someCustomer.name.get.first + " " + someCustomer.name.get.last),
        Fields.email -> Some(someCustomer.email.get),
        Fields.phone -> Some(someCustomer.phone.get),
        Fields.productDescription -> Some(someDeal.title.get))) errors "Unknown Merchant"

      tranzila.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        payment = somePayment,
        customer = Some(someCustomer),
        deal = Some(someDeal)) must beAFailedTry(check = beAnInstanceOf[PaymentErrorException])
    }

    "successfully yield an authorization key on valid request" in new Ctx {
      val someIndex = "some index"
      val someConfirmationCode = "some confirmation code"

      driver.aRequestFor(Map(
        Fields.supplier -> Some(someMerchant.username),
        Fields.sum -> Some(toTranzilaAmount(somePayment.currencyAmount.amount)),
        Fields.currency -> Some(toTranzilaCurrency(somePayment.currencyAmount.currency)),
        Fields.transactionMode -> Some(TransactionModes.VERIFY),
        Fields.ccNumber -> Some(someCreditCard.number),
        Fields.expDate -> Some(toTranzilaYearMonth(
          year = someCreditCard.expiration.year,
          month = someCreditCard.expiration.month)),
        Fields.cvv -> Some(someCreditCard.csc.get),
        Fields.holderId -> Some(someCreditCard.holderId.get),
        Fields.contact -> Some(someCustomer.name.get.first + " " + someCustomer.name.get.last),
        Fields.email -> Some(someCustomer.email.get),
        Fields.phone -> Some(someCustomer.phone.get),
        Fields.productDescription -> Some(someDeal.title.get))) returns Map(
            Fields.response -> StatusCodes.success,
            Fields.index -> someIndex,
            Fields.confirmationCode -> someConfirmationCode)

      val authorizationKey: Try[String] = tranzila.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        payment = somePayment,
        customer = Some(someCustomer),
        deal = Some(someDeal))

      authorizationKey must beASuccessfulTry(
        check = beAuthorizationKey(
          authorization = beAuthorization(
            index = ===(someIndex),
            confirmationCode = ===(someConfirmationCode))))
    }

    "gracefully fail on rejected card" in new Ctx {
      val responseCode: String = StatusCodes.rejected

      driver.aRequestFor(Map(
        Fields.supplier -> Some(someMerchant.username),
        Fields.sum -> Some(toTranzilaAmount(somePayment.currencyAmount.amount)),
        Fields.currency -> Some(toTranzilaCurrency(somePayment.currencyAmount.currency)),
        Fields.transactionMode -> Some(TransactionModes.VERIFY),
        Fields.ccNumber -> Some(someCreditCard.number),
        Fields.expDate -> Some(toTranzilaYearMonth(
          year = someCreditCard.expiration.year,
          month = someCreditCard.expiration.month)),
        Fields.cvv -> Some(someCreditCard.csc.get),
        Fields.holderId -> Some(someCreditCard.holderId.get),
        Fields.contact -> Some(someCustomer.name.get.first + " " + someCustomer.name.get.last),
        Fields.email -> Some(someCustomer.email.get),
        Fields.phone -> Some(someCustomer.phone.get),
        Fields.productDescription -> Some(someDeal.title.get))) returns Map(Fields.response -> responseCode)

      tranzila.authorize(
        merchantKey = merchantKey,
        creditCard = someCreditCard,
        payment = somePayment,
        customer = Some(someCustomer),
        deal = Some(someDeal)) must beAFailedTry.like {
          case e: PaymentRejectedException => e.message must contain(responseCode)
        }
    }
  }


  "capture request via Tranzila gateway" should {
    "successfully yield a transaction ID on valid request" in new Ctx {
      val someAuthorization = TranzilaAuthorization(
        index = "some index",
        confirmationCode = "some confirmation code")
      val authorizationKey: String = authorizationParser.stringify(someAuthorization)

      val someAmount = 11.1
      val someIndex = "some returned index"

      driver.aRequestFor(Map(
        Fields.supplier -> Some(someMerchant.username),
        Fields.sum -> Some(toTranzilaAmount(someAmount)),
        Fields.transactionMode -> Some(TransactionModes.FINAL),
        Fields.index -> Some(someAuthorization.index),
        Fields.authnr -> Some(someAuthorization.confirmationCode))) returns Map(
          Fields.response -> StatusCodes.success,
          Fields.index -> someIndex)

      tranzila.capture(
        merchantKey = merchantKey,
        authorizationKey = authorizationKey,
        amount = someAmount) must beASuccessfulTry(check = ===(someIndex))
    }
  }


  "voidAuthorization request via Tranzila gateway" should {
    "successfully yield a transaction ID on valid request" in new Ctx {
      val someAuthorization = TranzilaAuthorization(
        index = "some index",
        confirmationCode = "some confirmation code")
      val authorizationKey: String = authorizationParser.stringify(someAuthorization)

      tranzila.voidAuthorization(
        merchantKey = merchantKey,
        authorizationKey = authorizationKey) must beASuccessfulTry(check = ===(someAuthorization.index))
    }
  }


  step {
    driver.stop()
  }
}
