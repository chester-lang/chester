package typings.node.nodeColontestMod

import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

/**
  * Mocking timers is a technique commonly used in software testing to simulate and
  * control the behavior of timers, such as `setInterval` and `setTimeout`,
  * without actually waiting for the specified time intervals.
  *
  * The MockTimers API also allows for mocking of the `Date` constructor and
  * `setImmediate`/`clearImmediate` functions.
  *
  * The `MockTracker` provides a top-level `timers` export
  * which is a `MockTimers` instance.
  * @since v20.4.0
  * @experimental
  */
@js.native
trait MockTimers extends StObject {
  
  /**
    * Enables timer mocking for the specified timers.
    *
    * **Note:** When you enable mocking for a specific timer, its associated
    * clear function will also be implicitly mocked.
    *
    * **Note:** Mocking `Date` will affect the behavior of the mocked timers
    * as they use the same internal clock.
    *
    * Example usage without setting initial time:
    *
    * ```js
    * import { mock } from 'node:test';
    * mock.timers.enable({ apis: ['setInterval', 'Date'], now: 1234 });
    * ```
    *
    * The above example enables mocking for the `Date` constructor, `setInterval` timer and
    * implicitly mocks the `clearInterval` function. Only the `Date` constructor from `globalThis`,
    * `setInterval` and `clearInterval` functions from `node:timers`, `node:timers/promises`, and `globalThis` will be mocked.
    *
    * Example usage with initial time set
    *
    * ```js
    * import { mock } from 'node:test';
    * mock.timers.enable({ apis: ['Date'], now: 1000 });
    * ```
    *
    * Example usage with initial Date object as time set
    *
    * ```js
    * import { mock } from 'node:test';
    * mock.timers.enable({ apis: ['Date'], now: new Date() });
    * ```
    *
    * Alternatively, if you call `mock.timers.enable()` without any parameters:
    *
    * All timers (`'setInterval'`, `'clearInterval'`, `'Date'`, `'setImmediate'`, `'clearImmediate'`, `'setTimeout'`, and `'clearTimeout'`)
    * will be mocked.
    *
    * The `setInterval`, `clearInterval`, `setTimeout`, and `clearTimeout` functions from `node:timers`, `node:timers/promises`,
    * and `globalThis` will be mocked.
    * The `Date` constructor from `globalThis` will be mocked.
    *
    * If there is no initial epoch set, the initial date will be based on 0 in the Unix epoch. This is `January 1st, 1970, 00:00:00 UTC`. You can
    * set an initial date by passing a now property to the `.enable()` method. This value will be used as the initial date for the mocked Date
    * object. It can either be a positive integer, or another Date object.
    * @since v20.4.0
    */
  def enable(): Unit = js.native
  def enable(options: MockTimersOptions): Unit = js.native
  
  /**
    * This function restores the default behavior of all mocks that were previously
    * created by this `MockTimers` instance and disassociates the mocks
    * from the `MockTracker` instance.
    *
    * **Note:** After each test completes, this function is called on
    * the test context's `MockTracker`.
    *
    * ```js
    * import { mock } from 'node:test';
    * mock.timers.reset();
    * ```
    * @since v20.4.0
    */
  def reset(): Unit = js.native
  
  /**
    * Triggers all pending mocked timers immediately. If the `Date` object is also
    * mocked, it will also advance the `Date` object to the furthest timer's time.
    *
    * The example below triggers all pending timers immediately,
    * causing them to execute without any delay.
    *
    * ```js
    * import assert from 'node:assert';
    * import { test } from 'node:test';
    *
    * test('runAll functions following the given order', (context) => {
    *   context.mock.timers.enable({ apis: ['setTimeout', 'Date'] });
    *   const results = [];
    *   setTimeout(() => results.push(1), 9999);
    *
    *   // Notice that if both timers have the same timeout,
    *   // the order of execution is guaranteed
    *   setTimeout(() => results.push(3), 8888);
    *   setTimeout(() => results.push(2), 8888);
    *
    *   assert.deepStrictEqual(results, []);
    *
    *   context.mock.timers.runAll();
    *   assert.deepStrictEqual(results, [3, 2, 1]);
    *   // The Date object is also advanced to the furthest timer's time
    *   assert.strictEqual(Date.now(), 9999);
    * });
    * ```
    *
    * **Note:** The `runAll()` function is specifically designed for
    * triggering timers in the context of timer mocking.
    * It does not have any effect on real-time system
    * clocks or actual timers outside of the mocking environment.
    * @since v20.4.0
    */
  def runAll(): Unit = js.native
  
  /**
    * You can use the `.setTime()` method to manually move the mocked date to another time. This method only accepts a positive integer.
    * Note: This method will execute any mocked timers that are in the past from the new time.
    * In the below example we are setting a new time for the mocked date.
    * ```js
    * import assert from 'node:assert';
    * import { test } from 'node:test';
    * test('sets the time of a date object', (context) => {
    *   // Optionally choose what to mock
    *   context.mock.timers.enable({ apis: ['Date'], now: 100 });
    *   assert.strictEqual(Date.now(), 100);
    *   // Advance in time will also advance the date
    *   context.mock.timers.setTime(1000);
    *   context.mock.timers.tick(200);
    *   assert.strictEqual(Date.now(), 1200);
    * });
    * ```
    */
  def setTime(time: Double): Unit = js.native
  
  /**
    * Advances time for all mocked timers.
    *
    * **Note:** This diverges from how `setTimeout` in Node.js behaves and accepts
    * only positive numbers. In Node.js, `setTimeout` with negative numbers is
    * only supported for web compatibility reasons.
    *
    * The following example mocks a `setTimeout` function and
    * by using `.tick` advances in
    * time triggering all pending timers.
    *
    * ```js
    * import assert from 'node:assert';
    * import { test } from 'node:test';
    *
    * test('mocks setTimeout to be executed synchronously without having to actually wait for it', (context) => {
    *   const fn = context.mock.fn();
    *
    *   context.mock.timers.enable({ apis: ['setTimeout'] });
    *
    *   setTimeout(fn, 9999);
    *
    *   assert.strictEqual(fn.mock.callCount(), 0);
    *
    *   // Advance in time
    *   context.mock.timers.tick(9999);
    *
    *   assert.strictEqual(fn.mock.callCount(), 1);
    * });
    * ```
    *
    * Alternativelly, the `.tick` function can be called many times
    *
    * ```js
    * import assert from 'node:assert';
    * import { test } from 'node:test';
    *
    * test('mocks setTimeout to be executed synchronously without having to actually wait for it', (context) => {
    *   const fn = context.mock.fn();
    *   context.mock.timers.enable({ apis: ['setTimeout'] });
    *   const nineSecs = 9000;
    *   setTimeout(fn, nineSecs);
    *
    *   const twoSeconds = 3000;
    *   context.mock.timers.tick(twoSeconds);
    *   context.mock.timers.tick(twoSeconds);
    *   context.mock.timers.tick(twoSeconds);
    *
    *   assert.strictEqual(fn.mock.callCount(), 1);
    * });
    * ```
    *
    * Advancing time using `.tick` will also advance the time for any `Date` object
    * created after the mock was enabled (if `Date` was also set to be mocked).
    *
    * ```js
    * import assert from 'node:assert';
    * import { test } from 'node:test';
    *
    * test('mocks setTimeout to be executed synchronously without having to actually wait for it', (context) => {
    *   const fn = context.mock.fn();
    *
    *   context.mock.timers.enable({ apis: ['setTimeout', 'Date'] });
    *   setTimeout(fn, 9999);
    *
    *   assert.strictEqual(fn.mock.callCount(), 0);
    *   assert.strictEqual(Date.now(), 0);
    *
    *   // Advance in time
    *   context.mock.timers.tick(9999);
    *   assert.strictEqual(fn.mock.callCount(), 1);
    *   assert.strictEqual(Date.now(), 9999);
    * });
    * ```
    * @since v20.4.0
    */
  def tick(milliseconds: Double): Unit = js.native
}