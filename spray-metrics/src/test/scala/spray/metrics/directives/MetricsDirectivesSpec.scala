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

  // Drivers and Testers //{1
  // Counter success //{2
  def driveCounterSuccess(path: String, route: Route): Unit = {
    Get(path) ~> route ~> check {
      status === OK
    }
  }
  def testCounterSuccess(path: String, route: Route, metricRegistry: MetricRegistry, prefix: String): Unit = {
    driveCounterSuccess(path, route)
    assertCounters(prefix, metricRegistry, 1, 0, 0, 0)
  }
  // 20 Counter successes //{2
  def testCounter20Successes(path: String, route: Route, metricRegistry: MetricRegistry, prefix: String): Unit = {
    (1 to 20) foreach { _ ⇒
      driveCounterSuccess(path, route)
    }
    assertCounters(prefix, metricRegistry, 20, 0, 0, 0)
  }
  // Counter failure //{2
  def driveCounterFailure(path: String, route: Route): Unit = {
    Get(path) ~> route ~> check {
      status === InternalServerError
    }
  }
  def testCounterFailure(path: String, route: Route, metricRegistry: MetricRegistry, prefix: String): Unit = {
    driveCounterFailure(path, route)
    assertCounters(prefix, metricRegistry, 0, 1, 0, 0)
  }
  // Counter rejection //{2
  def driveCounterRejection(path: String, route: Route): Unit = {
    Post(path) ~> route
  }
  def testCounterRejection(path: String, route: Route, metricRegistry: MetricRegistry, prefix: String): Unit = {
    driveCounterRejection(path, route)
    assertCounters(prefix, metricRegistry, 0, 0, 1, 0)
  }
  // Counter exception //{2
  def driveCounterException(path: String, route: Route): Unit = {
    Get(path) ~> route
  }
  def testCounterException(path: String, route: Route, metricRegistry: MetricRegistry, prefix: String): Unit = {
    driveCounterException(path, route)
    assertCounters(prefix, metricRegistry, 0, 0, 0, 1)
  }

  // Timer success //{2
  def driveTimerSuccess(path: String, route: Route): Unit = {
    Get(path) ~> route ~> check {
      status === OK
    }
  }
  def testTimerSuccess(path: String, route: Route, metricRegistry: MetricRegistry, timerName: String): Unit = {
    driveTimerSuccess(path, route)
    assertTimer(timerName, metricRegistry)
  }
  // Timer failure //{2
  def driveTimerFailure(path: String, route: Route): Unit = {
    Get(path) ~> route ~> check {
      status === InternalServerError
    }
  }
  def testTimerFailure(path: String, route: Route, metricRegistry: MetricRegistry, timerName: String): Unit = {
    driveTimerFailure(path, route)
    assertTimer(timerName, metricRegistry)
  }
  // Timer rejection //{2
  def driveTimerRejection(path: String, route: Route): Unit = {
    Post(path) ~> route
  }
  def testTimerRejection(path: String, route: Route, metricRegistry: MetricRegistry, timerName: String): Unit = {
    driveTimerRejection(path, route)
    assertTimer(timerName, metricRegistry)
  }
  // Timer exception //{2
  def driveTimerException(path: String, route: Route): Unit = {
    Post(path) ~> route
  }
  def testTimerException(path: String, route: Route, metricRegistry: MetricRegistry, timerName: String): Unit = {
    driveTimerException(path, route)
    assertTimer(timerName, metricRegistry)
  }
  //}1

  "MetricsDirectives" should { //{1
    "increment the counter" in new RoutableMetrics { //{2
      val route = successCounter()
      testCounterSuccess("/200", route, metricRegistry, "counter")
    } //}2
    "increment the failure counter" in new RoutableMetrics { //{2
      val route = failureCounter()
      testCounterFailure("/500", route, metricRegistry, "counter")
    } //}2
    "increment the rejection counter" in new RoutableMetrics { //{2
      val route = rejectionCounter()
      testCounterRejection("/reject", route, metricRegistry, "counter")
    } //}2
    "increment the exception counter" in new RoutableMetrics { //{2
      val route = exceptionCounter()
      testCounterException("/throw", route, metricRegistry, "counter")
    } //}2
    "increment the counter 20 times" in new RoutableMetrics { //{2
      val route = successCounter()
      testCounter20Successes("/200", route, metricRegistry, "counter")
    } //}2
    "increment all counters" in new RoutableMetrics { //{2
      val route = allCounter()
      driveCounterSuccess("/200", route)
      driveCounterFailure("/500", route)
      driveCounterRejection("/reject", route)
      driveCounterException("/throw", route)
      assertCounters("counter", metricRegistry, 1, 1, 1, 1)
    } //}2
    "increment the uri counter" in new RoutableMetrics { //{2
      val route = successCounter(uri = true)
      testCounterSuccess("/200/with/uri", route, metricRegistry, "200.with.uri")
    } //}2
    "increment the failure uri counter" in new RoutableMetrics { //{2
      val route = failureCounter(true)
      testCounterFailure("/500/with/uri", route, metricRegistry, "500.with.uri")
    } //}2
    "increment the rejection uri counter" in new RoutableMetrics { //{2
      val route = rejectionCounter(true)
      testCounterRejection("/reject/with/uri", route, metricRegistry, "reject.with.uri")
    } //}2
    "increment the exception uri counter" in new RoutableMetrics { //{2
      val route = exceptionCounter(true)
      testCounterException("/throw/with/uri", route, metricRegistry, "throw.with.uri")
    } //}2
    "increment the uri counter 20 times" in new RoutableMetrics { //{2
      val route = successCounter(true)
      testCounter20Successes("/200/with/uri", route, metricRegistry, "200.with.uri")
    } //}2
    "increment all uri counters" in new RoutableMetrics { //{2
      val route = allCounter(true)
      driveCounterSuccess("/200/with/uri", route)
      driveCounterFailure("/500/with/uri", route)
      driveCounterRejection("/reject/with/uri", route)
      driveCounterException("/throw/with/uri", route)
      assertCounters("200.with.uri", metricRegistry, 1, 0, 0, 0)
      assertCounters("500.with.uri", metricRegistry, 0, 1, 0, 0)
      assertCounters("reject.with.uri", metricRegistry, 0, 0, 1, 0)
      assertCounters("throw.with.uri", metricRegistry, 0, 0, 0, 1)
    } //}2
    "modify the timer on success" in new RoutableMetrics { //{2
      val route = successTimer()
      testTimerSuccess("/200", route, metricRegistry, "timer")
    } //}2
    "modify the timer on failure" in new RoutableMetrics { //{2
      val route = failureTimer()
      testTimerFailure("/500", route, metricRegistry, "timer")
    } //}2
    "modify the timer on rejection" in new RoutableMetrics { //{2
      val route = rejectionTimer()
      testTimerRejection("/reject", route, metricRegistry, "timer")
    } //}2
    "modify the timer on exception" in new RoutableMetrics { //{2
      val route = exceptionTimer()
      testTimerException("/throw", route, metricRegistry, "timer")
    } //}2
    "modify the uri timer on success" in new RoutableMetrics { //{2
      val route = successTimer(true)
      testTimerSuccess("/200/with/uri", route, metricRegistry, "200.with.uri")
    } //}2
    "modify the uri timer on failure" in new RoutableMetrics { //{2
      val route = failureTimer(true)
      testTimerFailure("/500/with/uri", route, metricRegistry, "500.with.uri")
    } //}2
    "modify the uri timer on rejection" in new RoutableMetrics { //{2
      val route = rejectionTimer(true)
      testTimerRejection("/reject/with/uri", route, metricRegistry, "reject.with.uri")
    } //}2
    "modify the uri timer on exception" in new RoutableMetrics { //{2
      val route = exceptionTimer(true)
      testTimerException("/throw/with/uri", route, metricRegistry, "throw.with.uri")
    } //}2
  } //}1
}
// vim:fdl=1:
