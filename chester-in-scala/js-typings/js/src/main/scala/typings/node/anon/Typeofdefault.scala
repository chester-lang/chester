package typings.node.anon

import typings.node.nodeColontestMod.TestFn
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

@js.native
trait Typeofdefault extends StObject {
  
  /**
    * The `test()` function is the value imported from the `test` module. Each
    * invocation of this function results in reporting the test to the `TestsStream`.
    *
    * The `TestContext` object passed to the `fn` argument can be used to perform
    * actions related to the current test. Examples include skipping the test, adding
    * additional diagnostic information, or creating subtests.
    *
    * `test()` returns a `Promise` that fulfills once the test completes.
    * if `test()` is called within a suite, it fulfills immediately.
    * The return value can usually be discarded for top level tests.
    * However, the return value from subtests should be used to prevent the parent
    * test from finishing first and cancelling the subtest
    * as shown in the following example.
    *
    * ```js
    * test('top level test', async (t) => {
    *   // The setTimeout() in the following subtest would cause it to outlive its
    *   // parent test if 'await' is removed on the next line. Once the parent test
    *   // completes, it will cancel any outstanding subtests.
    *   await t.test('longer running subtest', async (t) => {
    *     return new Promise((resolve, reject) => {
    *       setTimeout(resolve, 1000);
    *     });
    *   });
    * });
    * ```
    *
    * The `timeout` option can be used to fail the test if it takes longer than `timeout` milliseconds to complete. However, it is not a reliable mechanism for
    * canceling tests because a running test might block the application thread and
    * thus prevent the scheduled cancellation.
    * @since v18.0.0, v16.17.0
    * @param name The name of the test, which is displayed when reporting test results.
    * Defaults to the `name` property of `fn`, or `'<anonymous>'` if `fn` does not have a name.
    * @param options Configuration options for the test.
    * @param fn The function under test. The first argument to this function is a {@link TestContext} object.
    * If the test uses callbacks, the callback function is passed as the second argument.
    * @return Fulfilled with `undefined` once the test completes, or immediately if the test runs within a suite.
    */
  def apply(): js.Promise[Unit] = js.native
  def apply(fn: TestFn): js.Promise[Unit] = js.native
  def apply(name: String): js.Promise[Unit] = js.native
  def apply(name: String, fn: TestFn): js.Promise[Unit] = js.native
  def apply(name: Unit, fn: TestFn): js.Promise[Unit] = js.native
}