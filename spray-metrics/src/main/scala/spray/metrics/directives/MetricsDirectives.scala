/*
 * Copyright (C) 2011-2013 spray.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.metrics
package directives

import akka.actor.Status.Failure

import scala.util.control.NonFatal

import spray.http.HttpResponse
import spray.routing.{ Directive0, Rejected, RequestContext, RouteConcatenation }

import com.codahale.metrics.{ MetricRegistry, Timer }

object CounterMetric {
  type CountIncrementer = (String, MetricRegistry) ⇒ Unit

  val nilIncrementer: CountIncrementer = { (_, _) ⇒ () }

  def inc(postfix: String)(prefix: String, metricRegistry: MetricRegistry): Unit =
    metricRegistry.counter(prefix + "." + postfix).inc()

  val incSuccesses = inc("successes") _
  val incFailures = inc("failures") _
  val incRejections = inc("rejections") _
  val incExceptions = inc("exceptions") _
}

sealed trait CounterBase {
  val metricRegistry: MetricRegistry
  val handleFailures: CounterMetric.CountIncrementer
  val handleRejections: CounterMetric.CountIncrementer
  val handleExceptions: CounterMetric.CountIncrementer
  val handleSuccesses: CounterMetric.CountIncrementer

  def buildAfter(key: String): Any ⇒ Any = { possibleRsp: Any ⇒
    possibleRsp match {
      case rsp: HttpResponse ⇒
        if (rsp.status.isFailure) handleFailures(key, metricRegistry)
        else handleSuccesses(key, metricRegistry)
      case Rejected(_) ⇒
        handleRejections(key, metricRegistry)
      case Failure(_) ⇒
        handleExceptions(key, metricRegistry)
    }
    possibleRsp
  }
}

case class CounterMetric(
    prefix: String,
    metricRegistry: MetricRegistry,
    handleFailures: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleRejections: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleExceptions: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleSuccesses: CounterMetric.CountIncrementer = CounterMetric.incSuccesses) extends CounterBase {

  import CounterMetric._
  import spray.routing.directives.BasicDirectives._

  val count: Directive0 = around { ctx ⇒ (ctx, buildAfter(prefix)) }

  def notCountingSuccesses: CounterMetric = copy(handleSuccesses = nilIncrementer)
  def countingRejections: CounterMetric = copy(handleRejections = incRejections)
  def countingFailures: CounterMetric = copy(handleFailures = incFailures)
  def countingExceptions: CounterMetric = copy(handleExceptions = incExceptions)
  def countingAll: CounterMetric =
    copy(handleFailures = incFailures, handleRejections = incRejections, handleExceptions = incExceptions, handleSuccesses = incSuccesses)
}

case class CounterMetricByUri(
    metricRegistry: MetricRegistry,
    handleFailures: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleRejections: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleExceptions: CounterMetric.CountIncrementer = CounterMetric.nilIncrementer,
    handleSuccesses: CounterMetric.CountIncrementer = CounterMetric.incSuccesses) extends CounterBase {

  import CounterMetric._
  import spray.routing.directives.BasicDirectives._

  val count: Directive0 = around { ctx ⇒
    val key = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
    (ctx, buildAfter(key))
  }

  def notCountingSuccesses: CounterMetricByUri = copy(handleSuccesses = nilIncrementer)
  def countingFailures: CounterMetricByUri = copy(handleFailures = incFailures)
  def countingRejections: CounterMetricByUri = copy(handleRejections = incRejections)
  def countingExceptions: CounterMetricByUri = copy(handleExceptions = incExceptions)
  def countingAll: CounterMetricByUri =
    copy(handleFailures = incFailures, handleRejections = incRejections, handleExceptions = incExceptions, handleSuccesses = incSuccesses)
}

sealed trait TimerBase {
  val metricRegistry: MetricRegistry

  def buildAfter(timerContext: Timer.Context): Any ⇒ Any = { possibleRsp: Any ⇒
    possibleRsp match {
      case _ ⇒
        timerContext.stop()
    }
    possibleRsp
  }
}

case class TimerMetric(timerName: String, metricRegistry: MetricRegistry) extends TimerBase {
  import spray.routing.directives.BasicDirectives._

  val time: Directive0 =
    around { ctx ⇒
      val timerContext = metricRegistry.timer(timerName).time()
      (ctx, buildAfter(timerContext))
    }
}

case class TimerMetricByUri(metricRegistry: MetricRegistry) extends TimerBase {
  import spray.routing.directives.BasicDirectives._

  val time: Directive0 =
    around { ctx ⇒
      val key = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
      val timerContext = metricRegistry.timer(key).time()
      (ctx, buildAfter(timerContext))
    }
}

case class MeterMetric(meterName: String, metricRegistry: MetricRegistry) {
  import spray.routing.directives.BasicDirectives._

  val meter: Directive0 = mapRequestContext { ctx ⇒
    metricRegistry.meter(meterName).mark()
    ctx
  }
}

case class MeterMetricByUri(metricRegistry: MetricRegistry) {
  import spray.routing.directives.BasicDirectives._

  val meter: Directive0 = mapRequestContext { ctx ⇒
    val meterName = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
    metricRegistry.meter(meterName).mark()
    ctx
  }
}

trait CodaHaleMetricsDirectiveFactory {
  val metricRegistry: MetricRegistry

  def counter(counterPrefix: String): CounterMetric = new CounterMetric(counterPrefix, metricRegistry)
  def allCounter(counterPrefix: String): CounterMetric = new CounterMetric(counterPrefix, metricRegistry).countingAll

  def counter: CounterMetricByUri = new CounterMetricByUri(metricRegistry)
  def allCounter: CounterMetricByUri = new CounterMetricByUri(metricRegistry).countingAll

  def timer(timerPrefix: String): TimerMetric = new TimerMetric(timerPrefix, metricRegistry)
  def timer: TimerMetricByUri = new TimerMetricByUri(metricRegistry)

  def meter(meterName: String): MeterMetric = new MeterMetric(meterName, metricRegistry)
  def meter: MeterMetricByUri = new MeterMetricByUri(metricRegistry)
}

object CodaHaleMetricsDirectiveFactory {
  def apply(registry: MetricRegistry) = new CodaHaleMetricsDirectiveFactory {
    val metricRegistry = registry
  }
}
