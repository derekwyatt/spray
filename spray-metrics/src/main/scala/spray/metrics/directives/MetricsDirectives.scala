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

import spray.http.HttpResponse
import spray.routing.{ Directive0, RequestContext, RouteConcatenation }

import com.codahale.metrics.MetricRegistry

trait NonMetricsDirectives {
  import spray.routing.directives.BasicDirectives._

  def around[A](before: RequestContext ⇒ (RequestContext, A))(after: A ⇒ HttpResponse ⇒ HttpResponse): Directive0 =
    mapRequestContext { ctx ⇒
      val (newCtx, a) = before(ctx)
      newCtx.withHttpResponseMapped(after(a))
    }
}

class CounterMetric(prefix: String, metricRegistry: MetricRegistry) { self ⇒
  import spray.routing.directives.BasicDirectives._
  import spray.routing.directives.ExecutionDirectives._

  val withContext: RequestContext ⇒ RequestContext = { ctx ⇒
    ctx.withHttpResponseMapped { rsp ⇒
      if (rsp.status.isFailure)
        metricRegistry.counter(s"$prefix.failures").inc()
      else
        metricRegistry.counter(s"$prefix.successes").inc()
      rsp
    }
  }

  val count: Directive0 = mapRequestContext { ctx ⇒ withContext(ctx) }

  def countingRejections: CounterMetric = new CounterMetric(prefix, metricRegistry) {
    override val withContext: RequestContext ⇒ RequestContext = { ctx ⇒
      self.withContext(ctx.withRejectionHandling { rejections ⇒
        metricRegistry.counter(s"$prefix.rejections").inc()
      })
    }
  }
}

class CounterMetricByUri(metricRegistry: MetricRegistry) extends NonMetricsDirectives {
  import spray.routing.directives.BasicDirectives._

  val count: Directive0 =
    around { ctx ⇒
      val key = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
      (ctx, key)
    } { key ⇒
      rsp ⇒
        if (rsp.status.isFailure)
          metricRegistry.counter(s"$key.failures").inc()
        else
          metricRegistry.counter(s"$key.successes").inc()
        rsp
    }
}

class TimerMetric(timerName: String, metricRegistry: MetricRegistry) extends NonMetricsDirectives {
  import spray.routing.directives.BasicDirectives._

  val time: Directive0 =
    around { ctx ⇒
      (ctx, metricRegistry.timer(timerName).time())
    } { timerContext ⇒
      rsp ⇒
        timerContext.stop()
        rsp
    }
}

class TimerMetricByUri(metricRegistry: MetricRegistry) extends NonMetricsDirectives {
  import spray.routing.directives.BasicDirectives._

  val time: Directive0 =
    around { ctx ⇒
      val key = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
      (ctx, metricRegistry.timer(key).time())
    } { timerContext ⇒
      rsp ⇒
        timerContext.stop()
        rsp
    }
}

trait CodaHaleMetricsDirectiveFactory {
  val metricRegistry: MetricRegistry

  def counter(counterPrefix: String): CounterMetric = new CounterMetric(counterPrefix, metricRegistry)
  def counter: CounterMetricByUri = new CounterMetricByUri(metricRegistry)
  def timer(timerPrefix: String): TimerMetric = new TimerMetric(timerPrefix, metricRegistry)
  def timer: TimerMetricByUri = new TimerMetricByUri(metricRegistry)
}

object CodaHaleMetricsDirectiveFactory {
  def apply(registry: MetricRegistry) = new CodaHaleMetricsDirectiveFactory {
    val metricRegistry = registry
  }
}
