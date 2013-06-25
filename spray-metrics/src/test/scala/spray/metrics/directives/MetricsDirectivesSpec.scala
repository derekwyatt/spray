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
import shapeless._
import spray.http.StatusCode
import spray.http.StatusCodes._
import spray.routing._
import spray.routing.directives._
import spray.testkit.Specs2RouteTest

import com.codahale.metrics.MetricRegistry

object MetricsDirectivesSpec {
  trait RoutableMetrics extends Scope with Directives {
    import spray.routing.PathMatcher._
    val metricRegistry = new MetricRegistry()
    val metricFactory = CodaHaleMetricsDirectiveFactory(metricRegistry)

    val counter = metricFactory.counter("counter")
    val counterByUri = metricFactory.counter
    val timer = metricFactory.timer("timer")
    val timerByUri = metricFactory.timer
    val meter = metricFactory.meter("meter")
    val meterByUri = metricFactory.meter

    def buildPathGet(statusCode: StatusCode, where: PathMatcher0, f: Directive0, throwOne: Boolean = false): Route =
      path(where) {
        f {
          get {
            dynamic {
              if (throwOne)
                throw new Exception("Blurgh!!!")
              else
                complete(statusCode)
            }
          }
        }
      }

    def buildCase(status: StatusCode,
                  where: PathMatcher0,
                  withoutUri: Directive0,
                  withUri: Directive0,
                  throwOne: Boolean,
                  uri: Boolean) = {
      val directive = if (uri) withUri else withoutUri
      val path = if (uri) where / "with" / "uri" else where
      buildPathGet(status, path, directive, throwOne)
    }

    // Counter Routes
    def successCounter(uri: Boolean = false) = buildCase(OK, "200", counter.count, counterByUri.count, false, uri)
    def failureCounter(uri: Boolean = false) = buildCase(InternalServerError,
      "500",
      counter.failures.count,
      counterByUri.failures.count,
      false,
      uri)
    def rejectionCounter(uri: Boolean = false) = buildCase(OK,
      "reject",
      counter.rejections.count,
      counterByUri.rejections.count,
      false,
      uri)
    def exceptionCounter(uri: Boolean = false) = buildCase(OK,
      "throw",
      counter.exceptions.count,
      counterByUri.exceptions.count,
      true,
      uri)
    def allCounter(uri: Boolean = false) = {
      val directive = if (!uri) counter.all.count else counterByUri.all.count
      buildCase(OK, "200", counter.all.count, counterByUri.all.count, false, uri) ~
        buildCase(InternalServerError, "500", counter.all.count, counterByUri.all.count, false, uri) ~
        buildCase(OK, "reject", counter.all.count, counterByUri.all.count, false, uri) ~
        buildCase(OK, "throw", counter.all.count, counterByUri.all.count, true, uri)
    }

    // Timer Routes
    def successTimer(uri: Boolean = false) = buildCase(OK, "200", timer.time, timerByUri.time, false, uri)
    def failureTimer(uri: Boolean = false) = buildCase(InternalServerError, "500", timer.time, timerByUri.time, false, uri)
    def rejectionTimer(uri: Boolean = false) = buildCase(OK, "reject", timer.time, timerByUri.time, false, uri)
    def exceptionTimer(uri: Boolean = false) = buildCase(OK, "throw", timer.time, timerByUri.time, true, uri)

    // Meter Routes
    def successMeter(uri: Boolean = false) = buildCase(OK, "200", meter.meter, meterByUri.meter, false, uri)
    def failureMeter(uri: Boolean = false) = buildCase(InternalServerError, "500", meter.meter, meterByUri.meter, false, uri)
    def rejectionMeter(uri: Boolean = false) = buildCase(OK, "reject", meter.meter, meterByUri.meter, false, uri)
    def exceptionMeter(uri: Boolean = false) = buildCase(OK, "throw", meter.meter, meterByUri.meter, true, uri)

    val route: Route
  }
}

class MetricsDirectivesSpec extends Specification with Specs2RouteTest {
  import MetricsDirectivesSpec._

  // Assertions
  def assertCounters(prefix: String, metricRegistry: MetricRegistry, successes: Int, failures: Int, rejections: Int, exceptions: Int) = {
    metricRegistry.counter(s"$prefix.successes").getCount() === successes
    metricRegistry.counter(s"$prefix.failures").getCount() === failures
    metricRegistry.counter(s"$prefix.rejections").getCount() === rejections
    metricRegistry.counter(s"$prefix.exceptions").getCount() === exceptions
  }
  def assertTimer(timerName: String, metricRegistry: MetricRegistry) = {
    metricRegistry.timer(timerName).getCount() === (1)
    metricRegistry.timer(timerName).getMeanRate() !== (0.0)
  }
  def assertMeter(meterName: String, metricRegistry: MetricRegistry) = {
    metricRegistry.meter(meterName).getCount() === (1)
    metricRegistry.meter(meterName).getMeanRate() !== (0.0)
  }

  // Overall driver
  def driveRoute(route: Route, uri: Boolean = false): Unit = {
    val postfix = if (uri) "/with/uri" else ""
    Get(s"/200$postfix") ~> route
    Get(s"/500$postfix") ~> route
    Post(s"/reject$postfix") ~> route
    Get(s"/throw$postfix") ~> route
  }

  // Counter Helpers
  def testCounterSuccess(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 1, 0, 0, 0)
  }

  def testCounter20Successes(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    (1 to 20) foreach { _ ⇒
      driveRoute(route, uri)
    }
    assertCounters(prefix, metricRegistry, 20, 0, 0, 0)
  }

  def testCounterFailure(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 0, 1, 0, 0)
  }

  def testCounterRejection(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 0, 0, 1, 0)
  }

  def testCounterException(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 0, 0, 0, 1)
  }

  // Timer Helpers
  def testTimerSuccess(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }

  def testTimerFailure(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }

  def testTimerRejection(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }

  def testTimerException(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }

  // Meter Helpers
  def testMeterSuccess(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }

  def testMeterFailure(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }

  def testMeterRejection(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }

  def testMeterException(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }

  "MetricsDirectives" should {
    "increment the counter" in new RoutableMetrics {
      val route = successCounter()
      testCounterSuccess(route, metricRegistry, "counter")
    }
    "increment the failure counter" in new RoutableMetrics {
      val route = failureCounter()
      testCounterFailure(route, metricRegistry, "counter")
    }
    "increment the rejection counter" in new RoutableMetrics {
      val route = rejectionCounter()
      testCounterRejection(route, metricRegistry, "counter")
    }
    "increment the exception counter" in new RoutableMetrics {
      val route = exceptionCounter()
      testCounterException(route, metricRegistry, "counter")
    }
    "increment the counter 20 times" in new RoutableMetrics {
      val route = successCounter()
      testCounter20Successes(route, metricRegistry, "counter")
    }
    "increment all counters" in new RoutableMetrics {
      val route = allCounter()
      driveRoute(route)
      assertCounters("counter", metricRegistry, 1, 1, 1, 1)
    }
    "increment the uri counter" in new RoutableMetrics {
      val route = successCounter(true)
      testCounterSuccess(route, metricRegistry, "200.with.uri.GET", true)
    }
    "increment the failure uri counter" in new RoutableMetrics {
      val route = failureCounter(true)
      testCounterFailure(route, metricRegistry, "500.with.uri.GET", true)
    }
    "increment the rejection uri counter" in new RoutableMetrics {
      val route = rejectionCounter(true)
      testCounterRejection(route, metricRegistry, "reject.with.uri.POST", true)
    }
    "increment the exception uri counter" in new RoutableMetrics {
      val route = exceptionCounter(true)
      testCounterException(route, metricRegistry, "throw.with.uri.GET", true)
    }
    "increment the uri counter 20 times" in new RoutableMetrics {
      val route = successCounter(true)
      testCounter20Successes(route, metricRegistry, "200.with.uri.GET", true)
    }
    "increment all uri counters" in new RoutableMetrics {
      val route = allCounter(true)
      driveRoute(route, true)
      assertCounters("200.with.uri.GET", metricRegistry, 1, 0, 0, 0)
      assertCounters("500.with.uri.GET", metricRegistry, 0, 1, 0, 0)
      assertCounters("reject.with.uri.POST", metricRegistry, 0, 0, 1, 0)
      assertCounters("throw.with.uri.GET", metricRegistry, 0, 0, 0, 1)
    }
    "modify the timer on success" in new RoutableMetrics {
      val route = successTimer()
      testTimerSuccess(route, metricRegistry, "timer")
    }
    "modify the timer on failure" in new RoutableMetrics {
      val route = failureTimer()
      testTimerFailure(route, metricRegistry, "timer")
    }
    "modify the timer on rejection" in new RoutableMetrics {
      val route = rejectionTimer()
      testTimerRejection(route, metricRegistry, "timer")
    }
    "modify the timer on exception" in new RoutableMetrics {
      val route = exceptionTimer()
      testTimerException(route, metricRegistry, "timer")
    }
    "modify the uri timer on success" in new RoutableMetrics {
      val route = successTimer(true)
      testTimerSuccess(route, metricRegistry, "200.with.uri.GET", true)
    }
    "modify the uri timer on failure" in new RoutableMetrics {
      val route = failureTimer(true)
      testTimerFailure(route, metricRegistry, "500.with.uri.GET", true)
    }
    "modify the uri timer on rejection" in new RoutableMetrics {
      val route = rejectionTimer(true)
      testTimerRejection(route, metricRegistry, "reject.with.uri.POST", true)
    }
    "modify the uri timer on exception" in new RoutableMetrics {
      val route = exceptionTimer(true)
      testTimerException(route, metricRegistry, "throw.with.uri.GET", true)
    }
    "modify the meter on success" in new RoutableMetrics {
      val route = successMeter()
      testMeterSuccess(route, metricRegistry, "meter")
    }
    "modify the meter on failure" in new RoutableMetrics {
      val route = failureMeter()
      testMeterFailure(route, metricRegistry, "meter")
    }
    "modify the meter on rejection" in new RoutableMetrics {
      val route = rejectionMeter()
      testMeterRejection(route, metricRegistry, "meter")
    }
    "modify the meter on exception" in new RoutableMetrics {
      val route = exceptionMeter()
      testMeterException(route, metricRegistry, "meter")
    }
    "modify the uri meter on success" in new RoutableMetrics {
      val route = successMeter(true)
      testMeterSuccess(route, metricRegistry, "200.with.uri.GET", true)
    }
    "modify the uri meter on failure" in new RoutableMetrics {
      val route = failureMeter(true)
      testMeterFailure(route, metricRegistry, "500.with.uri.GET", true)
    }
    "modify the uri meter on rejection" in new RoutableMetrics {
      val route = rejectionMeter(true)
      testMeterRejection(route, metricRegistry, "reject.with.uri.POST", true)
    }
    "modify the uri meter on exception" in new RoutableMetrics {
      val route = exceptionMeter(true)
      testMeterException(route, metricRegistry, "throw.with.uri.GET", true)
    }
  }
}
// vim:fdl=1:
