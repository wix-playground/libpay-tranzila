package com.wix.pay.tranzila.testkit


import scala.collection.JavaConversions._
import scala.collection.mutable
import java.io.ByteArrayOutputStream
import java.util.{List => JList}
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import com.google.api.client.http.{UrlEncodedContent, UrlEncodedParser}
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aStubWebServer


class TranzilaDriver(port: Int) {
  private val server: StubWebServer = aStubWebServer.onPort(port).build

  def start(): Unit = server.start()
  def stop(): Unit = server.stop()
  def reset(): Unit = server.replaceWith()


  def aRequestFor(params: Map[String, Option[String]]): RequestCtx = new RequestCtx(params)


  class RequestCtx(params: Map[String, Option[String]]) {
    def returns(responseParams: Map[String, String]) {
      server.appendAll {
        case HttpRequest(
        HttpMethods.POST,
        Path("/"),
        _,
        entity,
        _) if isStubbedRequest(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(
              ContentType(MediaTypes.`application/x-www-form-urlencoded`, HttpCharsets.`UTF-8`),
              urlEncode(responseParams)))
      }
    }

    def errors(message: String): Unit = {
      // Yeah, Tranzila returns broken HTML
      val responseHtml =
        s"""<html>
           |  <head>
           |    <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
           |  </head>
           |  <body>
           |  <center>
           |    <h1>Tranzila</h1>
           |    <br>
           |    An error ocurred with the following message:<br>
           |    <h3>
           |      <b><font color=red>$message</font></b>
           |      <br>
           |    </h3>""".stripMargin

      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/"),
          _,
          entity,
          _) if isStubbedRequest(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(ContentType(MediaTypes.`text/html`, HttpCharsets.`UTF-8`), responseHtml))
      }
    }

    private def isStubbedRequest(entity: HttpEntity): Boolean = {
      val requestParams = urlDecode(entity.extractAsString)
      val isAllStubbedParams = params.forall {
        case (k, v) => requestParams.contains(k) && v.fold(true)(_ == requestParams(k))
      }

      entity.contentType.mediaType == MediaTypes.`application/x-www-form-urlencoded` &&
        entity.contentType.charsetOption.isEmpty &&
        isAllStubbedParams
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
