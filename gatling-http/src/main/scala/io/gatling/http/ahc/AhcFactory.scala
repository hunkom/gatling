/*
 * Copyright 2011-2018 GatlingCorp (http://gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.gatling.http.ahc

import java.util.concurrent.TimeUnit
import javax.net.ssl.{ KeyManagerFactory, TrustManagerFactory }

import io.gatling.commons.util.Ssl._
import io.gatling.commons.util.SystemProps._
import io.gatling.core.config.AhcConfiguration
import io.gatling.core.{ ConfigKeys, CoreComponents }
import io.gatling.core.session.Session

import com.typesafe.scalalogging.StrictLogging
import io.netty.channel.EventLoopGroup
import io.netty.handler.ssl.{ SslContextBuilder, SslProvider }
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.util.internal.logging.{ InternalLoggerFactory, Slf4JLoggerFactory }
import io.netty.util.{ HashedWheelTimer, Timer }
import org.asynchttpclient.AsyncHttpClientConfig._
import org.asynchttpclient._

private[gatling] object AhcFactory {

  implicit class RichAsyncHttpClientConfigBuilder(val ahcConfigBuilder: DefaultAsyncHttpClientConfig.Builder) extends AnyVal {

    def setSslContext(ahcConfig: AhcConfiguration, keyManagerFactory: Option[KeyManagerFactory], trustManagerFactory: Option[TrustManagerFactory]): DefaultAsyncHttpClientConfig.Builder = {
      val sslContext = SslContextBuilder.forClient
        .sslProvider(if (ahcConfig.useOpenSsl) SslProvider.OPENSSL else SslProvider.JDK)
        .keyManager(keyManagerFactory.orNull)
        .trustManager(trustManagerFactory.orElse(if (ahcConfig.useInsecureTrustManager) Some(InsecureTrustManagerFactory.INSTANCE) else None).orNull)
        .sessionCacheSize(ahcConfig.sslSessionCacheSize)
        .sessionTimeout(ahcConfig.sslSessionTimeout)
        .build
      ahcConfigBuilder.setSslContext(sslContext)
    }
  }

  def apply(coreComponents: CoreComponents): AhcFactory =
    coreComponents.configuration.resolve(
      // [fl]
      //
      //
      //
      //
      //
      // [fl]
      new DefaultAhcFactory(coreComponents)
    )
}

private[gatling] trait AhcFactory {

  def defaultAhc: AsyncHttpClient

  def newAhc(session: Session): AsyncHttpClient
}

private[gatling] class DefaultAhcFactory(coreComponents: CoreComponents)
  extends NettyFactory(coreComponents.system)
  with AhcFactory
  with StrictLogging {

  import AhcFactory._

  private val configuration = coreComponents.configuration
  private val ahcConfig = configuration.http.ahc
  setSystemPropertyIfUndefined("io.netty.allocator.type", configuration.http.ahc.allocator)
  setSystemPropertyIfUndefined("io.netty.maxThreadLocalCharBufferSize", configuration.http.ahc.maxThreadLocalCharBufferSize)

  // set up Netty LoggerFactory for slf4j instead of default JDK
  InternalLoggerFactory.setDefaultFactory(Slf4JLoggerFactory.INSTANCE)

  private[this] def newTimer: Timer = {
    val timer = new HashedWheelTimer(10, TimeUnit.MILLISECONDS)
    timer.start()
    coreComponents.system.registerOnTermination(timer.stop())
    timer
  }

  private[gatling] def newAhcConfigBuilder(eventLoopGroup: EventLoopGroup, timer: Timer) = {
    val ahcConfigBuilder = new DefaultAsyncHttpClientConfig.Builder()
      .setKeepAlive(ahcConfig.keepAlive)
      .setConnectTimeout(ahcConfig.connectTimeout)
      .setHandshakeTimeout(ahcConfig.handshakeTimeout)
      .setPooledConnectionIdleTimeout(ahcConfig.pooledConnectionIdleTimeout)
      .setReadTimeout(ahcConfig.readTimeout)
      .setMaxRequestRetry(ahcConfig.maxRetry)
      .setRequestTimeout(ahcConfig.requestTimeOut)
      .setUseProxyProperties(false)
      .setUserAgent(null)
      .setEventLoopGroup(eventLoopGroup)
      .setNettyTimer(timer)
      .setResponseBodyPartFactory(ResponseBodyPartFactory.LAZY)
      .setDisableHttpsEndpointIdentificationAlgorithm(ahcConfig.disableHttpsEndpointIdentificationAlgorithm)
      .setUseInsecureTrustManager(ahcConfig.useInsecureTrustManager)
      .setFilterInsecureCipherSuites(ahcConfig.filterInsecureCipherSuites)
      .setUseLaxCookieEncoder(true)
      .setEnabledProtocols(ahcConfig.sslEnabledProtocols match {
        case Nil => null
        case ps  => ps.toArray
      })
      .setSslSessionCacheSize(ahcConfig.sslSessionCacheSize)
      .setSslSessionTimeout(ahcConfig.sslSessionTimeout)
      .setHttpClientCodecMaxInitialLineLength(Int.MaxValue)
      .setHttpClientCodecMaxHeaderSize(Int.MaxValue)
      .setHttpClientCodecMaxChunkSize(ahcConfig.httpClientCodecMaxChunkSize)
      .setHttpClientCodecInitialBufferSize(ahcConfig.httpClientCodecInitialBufferSize)
      .setKeepEncodingHeader(true)
      .setWebSocketMaxFrameSize(Int.MaxValue)
      .setUseOpenSsl(ahcConfig.useOpenSsl)
      .setUseNativeTransport(ahcConfig.useNativeTransport)
      .setValidateResponseHeaders(false)
      .setTcpNoDelay(ahcConfig.tcpNoDelay)
      .setSoReuseAddress(ahcConfig.soReuseAddress)
      .setSoLinger(ahcConfig.soLinger)
      .setSoSndBuf(ahcConfig.soSndBuf)
      .setSoRcvBuf(ahcConfig.soRcvBuf)
      .setCookieStore(null)

    if (ahcConfig.sslEnabledCipherSuites.nonEmpty) {
      ahcConfigBuilder.setEnabledCipherSuites(ahcConfig.sslEnabledCipherSuites.toArray)
    }

    val keyManagerFactory = configuration.http.ssl.keyStore
      .map(config => newKeyManagerFactory(config.storeType, config.file, config.password, config.algorithm))

    val trustManagerFactory = configuration.http.ssl.trustStore
      .map(config => newTrustManagerFactory(config.storeType, config.file, config.password, config.algorithm))

    if (keyManagerFactory.isDefined || trustManagerFactory.isDefined)
      ahcConfigBuilder.setSslContext(ahcConfig, keyManagerFactory, trustManagerFactory)

    ahcConfigBuilder
  }

  private[this] val defaultAhcConfig = {
    val eventLoopGroup = newEventLoopGroup("gatling-http-thread")
    val timer = newTimer
    val ahcConfigBuilder = newAhcConfigBuilder(eventLoopGroup, timer)
    ahcConfigBuilder.build
  }

  override val defaultAhc: AsyncHttpClient = newAhc(None)

  override def newAhc(session: Session): AsyncHttpClient = newAhc(Some(session))

  private[this] def newAhc(session: Option[Session]) = {
    val config = session.flatMap { session =>

      val keyManagerFactory = for {
        file <- session(ConfigKeys.http.ssl.keyStore.File).asOption[String]
        password <- session(ConfigKeys.http.ssl.keyStore.Password).asOption[String]
      } yield {
        val storeType = session(ConfigKeys.http.ssl.keyStore.Type).asOption[String]
        val algorithm = session(ConfigKeys.http.ssl.keyStore.Algorithm).asOption[String]
        newKeyManagerFactory(storeType, file, password, algorithm)
      }

      val trustManagerFactory = for {
        file <- session(ConfigKeys.http.ssl.trustStore.File).asOption[String]
        password <- session(ConfigKeys.http.ssl.trustStore.Password).asOption[String]
      } yield {
        val storeType = session(ConfigKeys.http.ssl.trustStore.Type).asOption[String]
        val algorithm = session(ConfigKeys.http.ssl.trustStore.Algorithm).asOption[String]
        newTrustManagerFactory(storeType, file, password, algorithm)
      }

      trustManagerFactory.orElse(keyManagerFactory).map { _ =>
        logger.info(s"Setting a custom SslContext for user ${session.userId}")
        new DefaultAsyncHttpClientConfig.Builder(defaultAhcConfig).setSslContext(ahcConfig, keyManagerFactory, trustManagerFactory).build
      }

    }.getOrElse(defaultAhcConfig)

    val client = new DefaultAsyncHttpClient(config)
    coreComponents.system.registerOnTermination(client.close())
    client
  }
}
