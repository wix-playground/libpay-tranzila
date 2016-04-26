package com.wix.pay.tranzila.testkit


import java.io.ByteArrayOutputStream
import java.util.{List => JList}

import com.google.api.client.http.{UrlEncodedContent, UrlEncodedParser}
import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import spray.http._

import scala.collection.JavaConversions._
import scala.collection.mutable

class TranzilaDriver(port: Int) {
  private val probe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)

  def startProbe() {
    probe.doStart()
  }

  def stopProbe() {
    probe.doStop()
  }

  def resetProbe() {
    probe.handlers.clear()
  }

  def aRequestFor(params: Map[String, Option[String]]): RequestCtx = {
    new RequestCtx(params)
  }

  class RequestCtx(params: Map[String, Option[String]]) {
    def returns(responseParams: Map[String, String]) {
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path("/"),
        headers,
        entity,
        _) if isStubbedRequest(headers, entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentType(MediaTypes.`application/x-www-form-urlencoded`), urlEncode(responseParams)))
      }
    }

    def errors(message: String): Unit = {
      // Yeah, Tranzila returns broken HTML
      val responseHtml =
        "<html><head><META NAME=\"ROBOTS\" CONTENT=\"NOINDEX, NOFOLLOW\"></head><body>\n<center><h1>Tranzila</h1><br>\nAn error ocurred with the following message:<br>\n<h3><b><font color=red> " +
        message + " </font></b><br></h3>"
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path("/"),
        headers,
        entity,
        _) if isStubbedRequest(headers, entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentType(MediaTypes.`text/html`), responseHtml))
      }
    }

    private def isStubbedRequest(headers: List[HttpHeader], entity: HttpEntity): Boolean = {
      isStubbedHeaders(headers) && isStubbedEntity(entity)
    }

    private def isStubbedHeaders(headers: List[HttpHeader]): Boolean = {
      // Tranzila expects this ContentType exactly, e.g. no charset part
      val contentType = headers.find((_:HttpHeader).name == "Content-Type")
      contentType.isDefined && contentType.get.value == "application/x-www-form-urlencoded"
    }

    private def isStubbedEntity(entity: HttpEntity): Boolean = {
      val requestParams = urlDecode(entity.asString)

      params.forall {
        case (k, v) => requestParams.contains(k) && v.fold(true)(_ == requestParams(k))
      }
    }

    private def urlEncode(params: Map[String, String]): String = {
      val baos = new ByteArrayOutputStream()
      new UrlEncodedContent(mapAsJavaMap(params)).writeTo(baos)
      new String(baos.toByteArray, "UTF-8")
    }

    private def urlDecode(str: String): Map[String, String] = {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(str, mutableMapAsJavaMap(params))
      params.mapValues( _(0) ).toMap
    }
  }
}
