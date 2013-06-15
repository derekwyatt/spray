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

package spray.metrics.directives

import akka.actor.ActorSystem
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import spray.http.StatusCodes._
import spray.routing.Directives
import spray.testkit.Specs2RouteTest

import com.codahale.metrics.MetricRegistry

object MetricsDirectivesSpec {
  class RoutableMetrics extends Scope with Directives with MetricsDirectives {
    val metricRegistry = new MetricRegistry()
    val route =
      dynamic {
        get {
          path("counter") {
            counted("counter") {
              complete(OK)
            }
          } ~
            path("counter" / "with" / "uri") {
              countedUri {
                complete(OK)
              }
            } ~
            path("meter") {
              metered("meter") {
                complete(OK)
              }
            } ~
            path("timer") {
              timed("timer") {
                Thread.sleep(20)
                complete(OK)
              }
            }
        }
      }
  }
}

class MetricsDirectivesSpec extends Specification with Specs2RouteTest {
  import MetricsDirectivesSpec._

  "MetricsDirectives" should { //{1
    "increment the counter on /counter" in new RoutableMetrics { //{2
      Get("/counter") ~> route ~> check {
        status === OK
        metricRegistry.counter("counter.success").getCount() === 1
      }
    } //}2
    "increment the uri counter" in new RoutableMetrics { //{2
      Get("/counter/with/uri") ~> route ~> check {
        status === (OK)
        metricRegistry.counter("counter.with.uri.success").getCount() === (1)
      }
    } //}2
    "increment the counter 20 times on /counter" in new RoutableMetrics { //{2
      (1 to 20) foreach { _ ⇒
        Get("/counter") ~> route ~> check {
          status === (OK)
        }
      }
      metricRegistry.counter("counter.success").getCount() === (20)
    } //}2
    "modify the meter" in new RoutableMetrics { //{2
      (1 to 50) foreach { _ ⇒
        Get("/meter") ~> route ~> check {
          status === (OK)
        }
      }
      metricRegistry.meter("meter").getCount() === (50)
      metricRegistry.meter("meter").getMeanRate() !=== (0.0)
    } //}2
    "modify the timer" in new RoutableMetrics { //{2
      Get("/timer") ~> route ~> check {
        status must be(OK)
        metricRegistry.timer("timer").getCount() === (1)
        metricRegistry.timer("timer").getMeanRate() !== (0.0)
      }
    } //}2
  } //}1
}
// vim:fdl=1:
