package com.wix.pay.tranzila


import java.util.{List => JList}

import com.google.api.client.http._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.shva.model.{IsShvaRejectedStatusCode, StatusCodes}
import com.wix.pay.tranzila.model.Conversions._
import com.wix.pay.tranzila.model._
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}

import scala.collection.JavaConversions._
import scala.collection.{JavaConversions, mutable}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


object Endpoints {
  /** Endpoint that accepts all transaction types, and expects UTF-8 encoding. */
  val production = "https://secure5.tranzila.com/cgi-bin/tranzila71u.cgi"
}

/**
 * Tranzila uses IP based filtering for authorization, which must be set in Tranzila's dashboard (use 0.0.0.0/0 to disable).
 */
class TranzilaGateway(requestFactory: HttpRequestFactory,
                      connectTimeout: Option[Duration] = None,
                      readTimeout: Option[Duration] = None,
                      numberOfRetries: Int = 0,
                      endpointUrl: String = Endpoints.production,
                      merchantParser: TranzilaMerchantParser = new JsonTranzilaMerchantParser,
                      authorizationParser: TranzilaAuthorizationParser = new JsonTranzilaAuthorizationParser) extends PaymentGateway {

  override def authorize(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)

      val request = createAuthorizeOrSaleRequest(
        transactionMode = TransactionModes.VERIFY,
        merchant = merchant,
        creditCard = creditCard,
        currencyAmount = currencyAmount,
        customer = customer,
        deal = deal
      )
      val response = doRequest(request)

      authorizationParser.stringify(TranzilaAuthorization(
        index = response(Fields.index),
        confirmationCode = response(Fields.confirmationCode)
      ))
    } match {
      case Success(authorizationKey) => Success(authorizationKey)
      case Failure(e: PaymentException) => Failure(e)
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  private def doRequest(params: Map[String, String]): Map[String, String] = {
    val content = new UrlEncodedContent(JavaConversions.mapAsJavaMap(params))

    // Workaround for Tranzila not handling "ContentType: application/x-www-form-urlencoded; charset=UTF-8"
    content.setMediaType(new HttpMediaType("application/x-www-form-urlencoded"))

    val httpRequest = requestFactory.buildPostRequest(new GenericUrl(endpointUrl), content)

    connectTimeout foreach (to => httpRequest.setConnectTimeout(to.toMillis.toInt))
    readTimeout foreach (to => httpRequest.setReadTimeout(to.toMillis.toInt))
    httpRequest.setNumberOfRetries(numberOfRetries)

    val response = extractAndCloseResponse(httpRequest.execute())
    verifyTranzilaResponse(response)
    response
  }

  private def extractAndCloseResponse(httpResponse: HttpResponse) = {
    try {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(httpResponse.parseAsString(), mutableMapAsJavaMap(params))
      params.mapValues( _(0) ).toMap
    } finally {
      httpResponse.ignore()
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)

      val request = createCaptureRequest(
        merchant = merchant,
        authorization = authorization,
        amount = amount
      )
      val response = doRequest(request)

      response(Fields.index)
    } match {
      case Success(numTrans) => Success(numTrans)
      case Failure(e: PaymentException) => Failure(e)
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)

      val request = createAuthorizeOrSaleRequest(
        transactionMode = TransactionModes.FINAL,
        merchant = merchant,
        creditCard = creditCard,
        currencyAmount = currencyAmount,
        customer = customer,
        deal = deal
      )
      val response = doRequest(request)

      response(Fields.index)
    } match {
      case Success(authorizationKey) => Success(authorizationKey)
      case Failure(e: PaymentException) => Failure(e)
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    Try {
//      val merchant = merchantParser.parse(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)

      // Tanzila doesn't support voiding an authorization. Authorizations are automatically voided after 2-21 days,
      // as set in Tranzila's dashboard (default is 2 days).
      authorization.index
    }
  }

  private def createAuthorizeOrSaleRequest(transactionMode: String, merchant: TranzilaMerchant, creditCard: CreditCard,
                                           currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Map[String, String] = {

    val params = mutable.Map(
      Fields.supplier -> merchant.username,
      Fields.sum -> toTranzilaAmount(currencyAmount.amount),
      Fields.currency -> toTranzilaCurrency(currencyAmount.currency),
      Fields.transactionMode -> transactionMode,
      Fields.ccNumber -> creditCard.number,
      Fields.expDate -> toTranzilaYearMonth(
        year = creditCard.expiration.year,
        month = creditCard.expiration.month
      )
    )

    creditCard.csc.foreach(params.put(Fields.cvv, _))
    creditCard.holderId.foreach(params.put(Fields.holderId, _))

    customer.foreach(customer => {
      customer.name.foreach(name => params.put(Fields.contact, s"${name.first} ${name.last}"))
      customer.email.foreach(email => params.put(Fields.email, email))
      customer.phone.foreach(phone => params.put(Fields.phone, phone))
    })

    deal.foreach(deal => {
      deal.title.foreach(title => params.put(Fields.productDescription, title))
    })

    params.toMap
  }

  private def createCaptureRequest(merchant: TranzilaMerchant, authorization: TranzilaAuthorization, amount: Double): Map[String, String] = {
    Map(
      Fields.supplier -> merchant.username,
      Fields.sum -> toTranzilaAmount(amount),
      Fields.transactionMode -> TransactionModes.FINAL,
      Fields.index -> authorization.index,
      Fields.authnr -> authorization.confirmationCode
    )
  }

  private def verifyTranzilaResponse(response: Map[String, String]): Unit = {
    val statusCode = response(Fields.response)

    statusCode match {
      case StatusCodes.success => // Operation successful.
      case IsShvaRejectedStatusCode(rejectedStatusCode) => throw new PaymentRejectedException(s"Status code: $rejectedStatusCode")
      case IsTranzilaRejectedStatusCode(rejectedStatusCode) => throw new PaymentRejectedException(s"Status code: $rejectedStatusCode")
      case _ => throw new PaymentErrorException(s"Status code: $statusCode")
    }
  }
}
