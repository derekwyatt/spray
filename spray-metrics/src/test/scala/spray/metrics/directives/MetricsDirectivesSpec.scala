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

    // buildPathGet //{2
    def buildPathGet(statusCode: StatusCode,
                     where: PathMatcher0,
                     f: Directive0,
                     throwOne: Boolean = false): Route =
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

    // buildCase //{2
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

    // Counter Routes //{2
    def successCounter(uri: Boolean = false) = buildCase(OK, "200", counter.count, counterByUri.count, false, uri)
    def failureCounter(uri: Boolean = false) = buildCase(InternalServerError,
      "500",
      counter.countingFailures.count,
      counterByUri.countingFailures.count,
      false,
      uri)
    def rejectionCounter(uri: Boolean = false) = buildCase(OK,
      "reject",
      counter.countingRejections.count,
      counterByUri.countingRejections.count,
      false,
      uri)
    def exceptionCounter(uri: Boolean = false) = buildCase(OK,
      "throw",
      counter.countingExceptions.count,
      counterByUri.countingExceptions.count,
      true,
      uri)
    def allCounter(uri: Boolean = false) = {
      val directive = if (!uri) counter.countingAll.count else counterByUri.countingAll.count
      buildCase(OK, "200", counter.countingAll.count, counterByUri.countingAll.count, false, uri) ~
        buildCase(InternalServerError, "500", counter.countingAll.count, counterByUri.countingAll.count, false, uri) ~
        buildCase(OK, "reject", counter.countingAll.count, counterByUri.countingAll.count, false, uri) ~
        buildCase(OK, "throw", counter.countingAll.count, counterByUri.countingAll.count, true, uri)
    }

    // Timer Routes //{2
    def successTimer(uri: Boolean = false) = buildCase(OK, "200", timer.time, timerByUri.time, false, uri)
    def failureTimer(uri: Boolean = false) = buildCase(InternalServerError, "500", timer.time, timerByUri.time, false, uri)
    def rejectionTimer(uri: Boolean = false) = buildCase(OK, "reject", timer.time, timerByUri.time, false, uri)
    def exceptionTimer(uri: Boolean = false) = buildCase(OK, "throw", timer.time, timerByUri.time, true, uri)

    // Meter Routes //{2
    def successMeter(uri: Boolean = false) = buildCase(OK, "200", meter.meter, meterByUri.meter, false, uri)
    def failureMeter(uri: Boolean = false) = buildCase(InternalServerError, "500", meter.meter, meterByUri.meter, false, uri)
    def rejectionMeter(uri: Boolean = false) = buildCase(OK, "reject", meter.meter, meterByUri.meter, false, uri)
    def exceptionMeter(uri: Boolean = false) = buildCase(OK, "throw", meter.meter, meterByUri.meter, true, uri)
    //}2

    val route: Route
  }
}

class MetricsDirectivesSpec extends Specification with Specs2RouteTest {
  import MetricsDirectivesSpec._

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

  // Drivers and Testers //{1
  // Route Driver //{2
  def driveRoute(route: Route, uri: Boolean = false): Unit = {
    val postfix = if (uri) "/with/uri" else ""
    Get(s"/200$postfix") ~> route
    Get(s"/500$postfix") ~> route
    Post(s"/reject$postfix") ~> route
    Get(s"/throw$postfix") ~> route
  }
  // Counter success //{2
  def testCounterSuccess(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 1, 0, 0, 0)
  }
  // 20 Counter successes //{2
  def testCounter20Successes(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    (1 to 20) foreach { _ ⇒
      driveRoute(route, uri)
    }
    assertCounters(prefix, metricRegistry, 20, 0, 0, 0)
  }
  // Counter failure //{2
  def testCounterFailure(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 0, 1, 0, 0)
  }
  // Counter rejection //{2
  def testCounterRejection(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 0, 0, 1, 0)
  }
  // Counter exception //{2
  def testCounterException(route: Route, metricRegistry: MetricRegistry, prefix: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertCounters(prefix, metricRegistry, 0, 0, 0, 1)
  }

  // Timer success //{2
  def testTimerSuccess(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }
  // Timer failure //{2
  def testTimerFailure(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }
  // Timer rejection //{2
  def testTimerRejection(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }
  // Timer exception //{2
  def testTimerException(route: Route, metricRegistry: MetricRegistry, timerName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertTimer(timerName, metricRegistry)
  }
  // Meter success //{2
  def testMeterSuccess(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }
  // Meter failure //{2
  def testMeterFailure(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }
  // Meter rejection //{2
  def testMeterRejection(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }
  // Meter exception //{2
  def testMeterException(route: Route, metricRegistry: MetricRegistry, meterName: String, uri: Boolean = false): Unit = {
    driveRoute(route, uri)
    assertMeter(meterName, metricRegistry)
  }
  //}1

  "MetricsDirectives" should { //{1
    "increment the counter" in new RoutableMetrics { //{2
      val route = successCounter()
      testCounterSuccess(route, metricRegistry, "counter")
    } //}2
    "increment the failure counter" in new RoutableMetrics { //{2
      val route = failureCounter()
      testCounterFailure(route, metricRegistry, "counter")
    } //}2
    "increment the rejection counter" in new RoutableMetrics { //{2
      val route = rejectionCounter()
      testCounterRejection(route, metricRegistry, "counter")
    } //}2
    "increment the exception counter" in new RoutableMetrics { //{2
      val route = exceptionCounter()
      testCounterException(route, metricRegistry, "counter")
    } //}2
    "increment the counter 20 times" in new RoutableMetrics { //{2
      val route = successCounter()
      testCounter20Successes(route, metricRegistry, "counter")
    } //}2
    "increment all counters" in new RoutableMetrics { //{2
      val route = allCounter()
      driveRoute(route)
      assertCounters("counter", metricRegistry, 1, 1, 1, 1)
    } //}2
    "increment the uri counter" in new RoutableMetrics { //{2
      val route = successCounter(true)
      testCounterSuccess(route, metricRegistry, "200.with.uri", true)
    } //}2
    "increment the failure uri counter" in new RoutableMetrics { //{2
      val route = failureCounter(true)
      testCounterFailure(route, metricRegistry, "500.with.uri", true)
    } //}2
    "increment the rejection uri counter" in new RoutableMetrics { //{2
      val route = rejectionCounter(true)
      testCounterRejection(route, metricRegistry, "reject.with.uri", true)
    } //}2
    "increment the exception uri counter" in new RoutableMetrics { //{2
      val route = exceptionCounter(true)
      testCounterException(route, metricRegistry, "throw.with.uri", true)
    } //}2
    "increment the uri counter 20 times" in new RoutableMetrics { //{2
      val route = successCounter(true)
      testCounter20Successes(route, metricRegistry, "200.with.uri", true)
    } //}2
    "increment all uri counters" in new RoutableMetrics { //{2
      val route = allCounter(true)
      driveRoute(route, true)
      assertCounters("200.with.uri", metricRegistry, 1, 0, 0, 0)
      assertCounters("500.with.uri", metricRegistry, 0, 1, 0, 0)
      assertCounters("reject.with.uri", metricRegistry, 0, 0, 1, 0)
      assertCounters("throw.with.uri", metricRegistry, 0, 0, 0, 1)
    } //}2
    "modify the timer on success" in new RoutableMetrics { //{2
      val route = successTimer()
      testTimerSuccess(route, metricRegistry, "timer")
    } //}2
    "modify the timer on failure" in new RoutableMetrics { //{2
      val route = failureTimer()
      testTimerFailure(route, metricRegistry, "timer")
    } //}2
    "modify the timer on rejection" in new RoutableMetrics { //{2
      val route = rejectionTimer()
      testTimerRejection(route, metricRegistry, "timer")
    } //}2
    "modify the timer on exception" in new RoutableMetrics { //{2
      val route = exceptionTimer()
      testTimerException(route, metricRegistry, "timer")
    } //}2
    "modify the uri timer on success" in new RoutableMetrics { //{2
      val route = successTimer(true)
      testTimerSuccess(route, metricRegistry, "200.with.uri", true)
    } //}2
    "modify the uri timer on failure" in new RoutableMetrics { //{2
      val route = failureTimer(true)
      testTimerFailure(route, metricRegistry, "500.with.uri", true)
    } //}2
    "modify the uri timer on rejection" in new RoutableMetrics { //{2
      val route = rejectionTimer(true)
      testTimerRejection(route, metricRegistry, "reject.with.uri", true)
    } //}2
    "modify the uri timer on exception" in new RoutableMetrics { //{2
      val route = exceptionTimer(true)
      testTimerException(route, metricRegistry, "throw.with.uri", true)
    } //}2
    "modify the meter on success" in new RoutableMetrics { //{2
      val route = successMeter()
      testMeterSuccess(route, metricRegistry, "meter")
    } //}2
    "modify the meter on failure" in new RoutableMetrics { //{2
      val route = failureMeter()
      testMeterFailure(route, metricRegistry, "meter")
    } //}2
    "modify the meter on rejection" in new RoutableMetrics { //{2
      val route = rejectionMeter()
      testMeterRejection(route, metricRegistry, "meter")
    } //}2
    "modify the meter on exception" in new RoutableMetrics { //{2
      val route = exceptionMeter()
      testMeterException(route, metricRegistry, "meter")
    } //}2
    "modify the uri meter on success" in new RoutableMetrics { //{2
      val route = successMeter(true)
      testMeterSuccess(route, metricRegistry, "200.with.uri", true)
    } //}2
    "modify the uri meter on failure" in new RoutableMetrics { //{2
      val route = failureMeter(true)
      testMeterFailure(route, metricRegistry, "500.with.uri", true)
    } //}2
    "modify the uri meter on rejection" in new RoutableMetrics { //{2
      val route = rejectionMeter(true)
      testMeterRejection(route, metricRegistry, "reject.with.uri", true)
    } //}2
    "modify the uri meter on exception" in new RoutableMetrics { //{2
      val route = exceptionMeter(true)
      testMeterException(route, metricRegistry, "throw.with.uri", true)
    } //}2
  } //}1
}
// vim:fdl=1:
