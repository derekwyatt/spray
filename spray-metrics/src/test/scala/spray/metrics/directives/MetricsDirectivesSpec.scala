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
  class RoutableMetrics extends Scope with Directives {
    val metricRegistry = new MetricRegistry()
    val metricFactory = CodaHaleMetricsDirectiveFactory(metricRegistry)

    val counter = metricFactory.counter("counter")
    val rejectionCounter = metricFactory.counter("counter").countingRejections
    val counterByUri = metricFactory.counter
    val timer = metricFactory.timer("timer")
    val timerByUri = metricFactory.timer

    val route =
      path("counter") {
        get {
          counter.count {
            complete(OK)
          }
        }
      } ~
        path("failedcounter") {
          get {
            counter.count {
              complete(InternalServerError)
            }
          }
        } ~
        path("rejectioncounter") {
          rejectionCounter.count {
            get {
              complete(OK)
            }
          }
        } ~
        path("counter" / "with" / "uri") {
          get {
            counterByUri.count {
              complete(OK)
            }
          }
        } ~
        path("timer") {
          get {
            timer.time {
              Thread.sleep(20)
              complete(OK)
            }
          }
        } ~
        path("timer" / "with" / "uri") {
          get {
            timerByUri.time {
              Thread.sleep(20)
              complete(OK)
            }
          }
        }
  }
}

class MetricsDirectivesSpec extends Specification with Specs2RouteTest {
  import MetricsDirectivesSpec._

  def assertCounters(metricRegistry: MetricRegistry, successes: Int, failures: Int, rejections: Int) = {
    metricRegistry.counter("counter.successes").getCount() === successes
    metricRegistry.counter("counter.failures").getCount() === failures
    metricRegistry.counter("counter.rejections").getCount() === rejections
  }
  "MetricsDirectives" should { //{1
    "increment the counter on /counter" in new RoutableMetrics { //{2
      Get("/counter") ~> route ~> check {
        status === OK
        assertCounters(metricRegistry, 1, 0, 0)
      }
    } //}2
    "increment the failure counter on /failedcounter" in new RoutableMetrics { //{2
      Get("/failedcounter") ~> route ~> check {
        status === InternalServerError
        assertCounters(metricRegistry, 0, 1, 0)
      }
    } //}2
    "increment the rejection counter on /rejectioncounter" in new RoutableMetrics { //{2
      Post("/rejectioncounter") ~> route ~> check {
        assertCounters(metricRegistry, 0, 0, 1)
      }
    } //}2
    "increment the counter 20 times on /counter" in new RoutableMetrics { //{2
      (1 to 20) foreach { _ ⇒
        Get("/counter") ~> route ~> check {
          status === (OK)
        }
      }
      assertCounters(metricRegistry, 20, 0, 0)
    } //}2
    "increment the uri counter" in new RoutableMetrics { //{2
      Get("/counter/with/uri") ~> route ~> check {
        status === (OK)
        metricRegistry.counter("counter.with.uri.successes").getCount() === (1)
      }
    } //}2
    "modify the timer" in new RoutableMetrics { //{2
      Get("/timer") ~> route ~> check {
        status === OK
        metricRegistry.timer("timer").getCount() === (1)
        metricRegistry.timer("timer").getMeanRate() !== (0.0)
      }
    } //}2
    "modify the uri timer" in new RoutableMetrics { //{2
      Get("/timer/with/uri") ~> route ~> check {
        status === OK
        metricRegistry.timer("timer.with.uri").getCount() === (1)
        metricRegistry.timer("timer.with.uri").getMeanRate() !== (0.0)
      }
    } //}2
  } //}1
}
// vim:fdl=1:
