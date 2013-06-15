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
import spray.routing.{ Directive0, RequestContext }

import com.codahale.metrics.MetricRegistry

trait MetricsDirectives {
  import spray.routing.directives.BasicDirectives._

  val metricRegistry: MetricRegistry

  def around[A](before: RequestContext ⇒ (RequestContext, A))(after: A ⇒ HttpResponse ⇒ HttpResponse): Directive0 =
    mapRequestContext { ctx ⇒
      val (newCtx, a) = before(ctx)
      newCtx.withHttpResponseMapped(after(a))
    }

  def counted(counterPrefix: String): Directive0 =
    mapHttpResponse { rsp ⇒
      if (rsp.status.isFailure)
        metricRegistry.counter(s"$counterPrefix.failure").inc()
      else
        metricRegistry.counter(s"$counterPrefix.success").inc()
      rsp
    }

  def countedUri: Directive0 =
    around { ctx ⇒
      val key = ctx.request.uri.toString.drop(1).replaceAll("/", ".")
      (ctx, key)
    } { key ⇒
      rsp ⇒
        if (rsp.status.isFailure)
          metricRegistry.counter(s"$key.failure").inc()
        else
          metricRegistry.counter(s"$key.success").inc()
        rsp
    }

  def countFailed(counterName: String): Directive0 =
    mapHttpResponse { rsp ⇒
      if (rsp.status.isFailure)
        metricRegistry.counter(counterName).inc()
      rsp
    }

  def metered(meterName: String): Directive0 =
    mapInnerRoute { route ⇒
      metricRegistry.meter(meterName).mark()
      route
    }

  def timed(timerName: String): Directive0 =
    around { ctx ⇒
      (ctx, metricRegistry.timer(timerName).time())
    } { timerContext ⇒
      rsp ⇒
        timerContext.stop()
        rsp
    }
}

object MetricsDirectives {
  def apply(registry: MetricRegistry) = new MetricsDirectives {
    val metricRegistry = registry
  }
}
