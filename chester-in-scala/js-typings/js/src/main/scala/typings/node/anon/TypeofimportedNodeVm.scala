package typings.node.anon

import org.scalablytyped.runtime.Instantiable0
import org.scalablytyped.runtime.Instantiable2
import org.scalablytyped.runtime.Instantiable3
import typings.node.vmMod.CompileFunctionOptions
import typings.node.vmMod.Context
import typings.node.vmMod.CreateContextOptions
import typings.node.vmMod.MeasureMemoryOptions
import typings.node.vmMod.MemoryMeasurement
import typings.node.vmMod.RunningCodeInNewContextOptions
import typings.node.vmMod.RunningCodeOptions
import typings.node.vmMod.ScriptOptions
import typings.node.vmMod.SourceTextModuleOptions
import typings.node.vmMod.SyntheticModuleOptions
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

@js.native
trait TypeofimportedNodeVm extends StObject {
  
  /**
    * This feature is only available with the `--experimental-vm-modules` command
    * flag enabled.
    *
    * The `vm.Module` class provides a low-level interface for using
    * ECMAScript modules in VM contexts. It is the counterpart of the `vm.Script` class that closely mirrors [Module Record](https://262.ecma-international.org/14.0/#sec-abstract-module-records) s as
    * defined in the ECMAScript
    * specification.
    *
    * Unlike `vm.Script` however, every `vm.Module` object is bound to a context from
    * its creation. Operations on `vm.Module` objects are intrinsically asynchronous,
    * in contrast with the synchronous nature of `vm.Script` objects. The use of
    * 'async' functions can help with manipulating `vm.Module` objects.
    *
    * Using a `vm.Module` object requires three distinct steps: creation/parsing,
    * linking, and evaluation. These three steps are illustrated in the following
    * example.
    *
    * This implementation lies at a lower level than the `ECMAScript Module
    * loader`. There is also no way to interact with the Loader yet, though
    * support is planned.
    *
    * ```js
    * import vm from 'node:vm';
    *
    * const contextifiedObject = vm.createContext({
    *   secret: 42,
    *   print: console.log,
    * });
    *
    * // Step 1
    * //
    * // Create a Module by constructing a new `vm.SourceTextModule` object. This
    * // parses the provided source text, throwing a `SyntaxError` if anything goes
    * // wrong. By default, a Module is created in the top context. But here, we
    * // specify `contextifiedObject` as the context this Module belongs to.
    * //
    * // Here, we attempt to obtain the default export from the module "foo", and
    * // put it into local binding "secret".
    *
    * const bar = new vm.SourceTextModule(`
    *   import s from 'foo';
    *   s;
    *   print(s);
    * `, { context: contextifiedObject });
    *
    * // Step 2
    * //
    * // "Link" the imported dependencies of this Module to it.
    * //
    * // The provided linking callback (the "linker") accepts two arguments: the
    * // parent module (`bar` in this case) and the string that is the specifier of
    * // the imported module. The callback is expected to return a Module that
    * // corresponds to the provided specifier, with certain requirements documented
    * // in `module.link()`.
    * //
    * // If linking has not started for the returned Module, the same linker
    * // callback will be called on the returned Module.
    * //
    * // Even top-level Modules without dependencies must be explicitly linked. The
    * // callback provided would never be called, however.
    * //
    * // The link() method returns a Promise that will be resolved when all the
    * // Promises returned by the linker resolve.
    * //
    * // Note: This is a contrived example in that the linker function creates a new
    * // "foo" module every time it is called. In a full-fledged module system, a
    * // cache would probably be used to avoid duplicated modules.
    *
    * async function linker(specifier, referencingModule) {
    *   if (specifier === 'foo') {
    *     return new vm.SourceTextModule(`
    *       // The "secret" variable refers to the global variable we added to
    *       // "contextifiedObject" when creating the context.
    *       export default secret;
    *     `, { context: referencingModule.context });
    *
    *     // Using `contextifiedObject` instead of `referencingModule.context`
    *     // here would work as well.
    *   }
    *   throw new Error(`Unable to resolve dependency: ${specifier}`);
    * }
    * await bar.link(linker);
    *
    * // Step 3
    * //
    * // Evaluate the Module. The evaluate() method returns a promise which will
    * // resolve after the module has finished evaluating.
    *
    * // Prints 42.
    * await bar.evaluate();
    * ```
    * @since v13.0.0, v12.16.0
    * @experimental
    */
  var Module: Instantiable0[typings.node.nodeColonvmMod.Module] = js.native
  
  /**
    * Instances of the `vm.Script` class contain precompiled scripts that can be
    * executed in specific contexts.
    * @since v0.3.1
    */
  var Script: Instantiable2[
    /* code */ String, 
    /* options */ js.UndefOr[ScriptOptions | String], 
    typings.node.nodeColonvmMod.Script
  ] = js.native
  
  /**
    * This feature is only available with the `--experimental-vm-modules` command
    * flag enabled.
    *
    * The `vm.SourceTextModule` class provides the [Source Text Module Record](https://tc39.es/ecma262/#sec-source-text-module-records) as
    * defined in the ECMAScript specification.
    * @since v9.6.0
    * @experimental
    */
  var SourceTextModule: Instantiable2[
    /* code */ String, 
    /* options */ js.UndefOr[SourceTextModuleOptions], 
    typings.node.nodeColonvmMod.SourceTextModule
  ] = js.native
  
  /**
    * This feature is only available with the `--experimental-vm-modules` command
    * flag enabled.
    *
    * The `vm.SyntheticModule` class provides the [Synthetic Module Record](https://heycam.github.io/webidl/#synthetic-module-records) as
    * defined in the WebIDL specification. The purpose of synthetic modules is to
    * provide a generic interface for exposing non-JavaScript sources to ECMAScript
    * module graphs.
    *
    * ```js
    * const vm = require('node:vm');
    *
    * const source = '{ "a": 1 }';
    * const module = new vm.SyntheticModule(['default'], function() {
    *   const obj = JSON.parse(source);
    *   this.setExport('default', obj);
    * });
    *
    * // Use `module` in linking...
    * ```
    * @since v13.0.0, v12.16.0
    * @experimental
    */
  var SyntheticModule: Instantiable3[
    /* exportNames */ js.Array[String], 
    /* evaluateCallback */ js.ThisFunction0[/* this */ typings.node.vmMod.SyntheticModule, Unit], 
    /* options */ js.UndefOr[SyntheticModuleOptions], 
    typings.node.nodeColonvmMod.SyntheticModule
  ] = js.native
  
  /**
    * Compiles the given code into the provided context (if no context is
    * supplied, the current context is used), and returns it wrapped inside a
    * function with the given `params`.
    * @since v10.10.0
    * @param code The body of the function to compile.
    * @param params An array of strings containing all parameters for the function.
    */
  def compileFunction(code: String): FunctioncachedDataScriptc = js.native
  def compileFunction(code: String, params: js.Array[String]): FunctioncachedDataScriptc = js.native
  def compileFunction(code: String, params: js.Array[String], options: CompileFunctionOptions): FunctioncachedDataScriptc = js.native
  def compileFunction(code: String, params: Unit, options: CompileFunctionOptions): FunctioncachedDataScriptc = js.native
  
  /**
    * Returns an object containing commonly used constants for VM operations.
    * @since v20.12.0
    */
  val constants: TypeofconstantsUSEMAINCONTEXTDEFAULTLOADER = js.native
  
  /**
    * If given a `contextObject`, the `vm.createContext()` method will
    * [prepare that object](https://nodejs.org/docs/latest-v22.x/api/vm.html#what-does-it-mean-to-contextify-an-object)
    * and return a reference to it so that it can be used in `{@link runInContext}` or
    * [`script.runInContext()`](https://nodejs.org/docs/latest-v22.x/api/vm.html#scriptrunincontextcontextifiedobject-options). Inside such
    * scripts, the `contextObject` will be the global object, retaining all of its
    * existing properties but also having the built-in objects and functions any
    * standard [global object](https://es5.github.io/#x15.1) has. Outside of scripts run by the vm module, global
    * variables will remain unchanged.
    *
    * ```js
    * const vm = require('node:vm');
    *
    * global.globalVar = 3;
    *
    * const context = { globalVar: 1 };
    * vm.createContext(context);
    *
    * vm.runInContext('globalVar *= 2;', context);
    *
    * console.log(context);
    * // Prints: { globalVar: 2 }
    *
    * console.log(global.globalVar);
    * // Prints: 3
    * ```
    *
    * If `contextObject` is omitted (or passed explicitly as `undefined`), a new,
    * empty `contextified` object will be returned.
    *
    * The `vm.createContext()` method is primarily useful for creating a single
    * context that can be used to run multiple scripts. For instance, if emulating a
    * web browser, the method can be used to create a single context representing a
    * window's global object, then run all `<script>` tags together within that
    * context.
    *
    * The provided `name` and `origin` of the context are made visible through the
    * Inspector API.
    * @since v0.3.1
    * @return contextified object.
    */
  def createContext(): Context = js.native
  def createContext(sandbox: Unit, options: CreateContextOptions): Context = js.native
  def createContext(sandbox: Context): Context = js.native
  def createContext(sandbox: Context, options: CreateContextOptions): Context = js.native
  
  /**
    * Returns `true` if the given `object` object has been `contextified` using {@link createContext}.
    * @since v0.11.7
    */
  def isContext(sandbox: Context): Boolean = js.native
  
  /**
    * Measure the memory known to V8 and used by all contexts known to the
    * current V8 isolate, or the main context.
    *
    * The format of the object that the returned Promise may resolve with is
    * specific to the V8 engine and may change from one version of V8 to the next.
    *
    * The returned result is different from the statistics returned by `v8.getHeapSpaceStatistics()` in that `vm.measureMemory()` measure the
    * memory reachable by each V8 specific contexts in the current instance of
    * the V8 engine, while the result of `v8.getHeapSpaceStatistics()` measure
    * the memory occupied by each heap space in the current V8 instance.
    *
    * ```js
    * const vm = require('node:vm');
    * // Measure the memory used by the main context.
    * vm.measureMemory({ mode: 'summary' })
    *   // This is the same as vm.measureMemory()
    *   .then((result) => {
    *     // The current format is:
    *     // {
    *     //   total: {
    *     //      jsMemoryEstimate: 2418479, jsMemoryRange: [ 2418479, 2745799 ]
    *     //    }
    *     // }
    *     console.log(result);
    *   });
    *
    * const context = vm.createContext({ a: 1 });
    * vm.measureMemory({ mode: 'detailed', execution: 'eager' })
    *   .then((result) => {
    *     // Reference the context here so that it won't be GC'ed
    *     // until the measurement is complete.
    *     console.log(context.a);
    *     // {
    *     //   total: {
    *     //     jsMemoryEstimate: 2574732,
    *     //     jsMemoryRange: [ 2574732, 2904372 ]
    *     //   },
    *     //   current: {
    *     //     jsMemoryEstimate: 2438996,
    *     //     jsMemoryRange: [ 2438996, 2768636 ]
    *     //   },
    *     //   other: [
    *     //     {
    *     //       jsMemoryEstimate: 135736,
    *     //       jsMemoryRange: [ 135736, 465376 ]
    *     //     }
    *     //   ]
    *     // }
    *     console.log(result);
    *   });
    * ```
    * @since v13.10.0
    * @experimental
    */
  def measureMemory(): js.Promise[MemoryMeasurement] = js.native
  def measureMemory(options: MeasureMemoryOptions): js.Promise[MemoryMeasurement] = js.native
  
  /**
    * The `vm.runInContext()` method compiles `code`, runs it within the context of
    * the `contextifiedObject`, then returns the result. Running code does not have
    * access to the local scope. The `contextifiedObject` object _must_ have been
    * previously `contextified` using the {@link createContext} method.
    *
    * If `options` is a string, then it specifies the filename.
    *
    * The following example compiles and executes different scripts using a single `contextified` object:
    *
    * ```js
    * const vm = require('node:vm');
    *
    * const contextObject = { globalVar: 1 };
    * vm.createContext(contextObject);
    *
    * for (let i = 0; i < 10; ++i) {
    *   vm.runInContext('globalVar *= 2;', contextObject);
    * }
    * console.log(contextObject);
    * // Prints: { globalVar: 1024 }
    * ```
    * @since v0.3.1
    * @param code The JavaScript code to compile and run.
    * @param contextifiedObject The `contextified` object that will be used as the `global` when the `code` is compiled and run.
    * @return the result of the very last statement executed in the script.
    */
  def runInContext(code: String, contextifiedObject: Context): Any = js.native
  def runInContext(code: String, contextifiedObject: Context, options: String): Any = js.native
  def runInContext(code: String, contextifiedObject: Context, options: RunningCodeOptions): Any = js.native
  
  /**
    * The `vm.runInNewContext()` first contextifies the given `contextObject` (or
    * creates a new `contextObject` if passed as `undefined`), compiles the `code`,
    * runs it within the created context, then returns the result. Running code
    * does not have access to the local scope.
    *
    * If `options` is a string, then it specifies the filename.
    *
    * The following example compiles and executes code that increments a global
    * variable and sets a new one. These globals are contained in the `contextObject`.
    *
    * ```js
    * const vm = require('node:vm');
    *
    * const contextObject = {
    *   animal: 'cat',
    *   count: 2,
    * };
    *
    * vm.runInNewContext('count += 1; name = "kitty"', contextObject);
    * console.log(contextObject);
    * // Prints: { animal: 'cat', count: 3, name: 'kitty' }
    * ```
    * @since v0.3.1
    * @param code The JavaScript code to compile and run.
    * @param contextObject An object that will be `contextified`. If `undefined`, a new object will be created.
    * @return the result of the very last statement executed in the script.
    */
  def runInNewContext(code: String): Any = js.native
  def runInNewContext(code: String, contextObject: Unit, options: String): Any = js.native
  def runInNewContext(code: String, contextObject: Unit, options: RunningCodeInNewContextOptions): Any = js.native
  def runInNewContext(code: String, contextObject: Context): Any = js.native
  def runInNewContext(code: String, contextObject: Context, options: String): Any = js.native
  def runInNewContext(code: String, contextObject: Context, options: RunningCodeInNewContextOptions): Any = js.native
  
  /**
    * `vm.runInThisContext()` compiles `code`, runs it within the context of the
    * current `global` and returns the result. Running code does not have access to
    * local scope, but does have access to the current `global` object.
    *
    * If `options` is a string, then it specifies the filename.
    *
    * The following example illustrates using both `vm.runInThisContext()` and
    * the JavaScript [`eval()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval) function to run the same code:
    *
    * ```js
    * const vm = require('node:vm');
    * let localVar = 'initial value';
    *
    * const vmResult = vm.runInThisContext('localVar = "vm";');
    * console.log(`vmResult: '${vmResult}', localVar: '${localVar}'`);
    * // Prints: vmResult: 'vm', localVar: 'initial value'
    *
    * const evalResult = eval('localVar = "eval";');
    * console.log(`evalResult: '${evalResult}', localVar: '${localVar}'`);
    * // Prints: evalResult: 'eval', localVar: 'eval'
    * ```
    *
    * Because `vm.runInThisContext()` does not have access to the local scope, `localVar` is unchanged. In contrast,
    * [`eval()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval) _does_ have access to the
    * local scope, so the value `localVar` is changed. In this way `vm.runInThisContext()` is much like an [indirect `eval()` call](https://es5.github.io/#x10.4.2), e.g.`(0,eval)('code')`.
    *
    * ## Example: Running an HTTP server within a VM
    *
    * When using either `script.runInThisContext()` or {@link runInThisContext}, the code is executed within the current V8 global
    * context. The code passed to this VM context will have its own isolated scope.
    *
    * In order to run a simple web server using the `node:http` module the code passed
    * to the context must either call `require('node:http')` on its own, or have a
    * reference to the `node:http` module passed to it. For instance:
    *
    * ```js
    * 'use strict';
    * const vm = require('node:vm');
    *
    * const code = `
    * ((require) => {
    *   const http = require('node:http');
    *
    *   http.createServer((request, response) => {
    *     response.writeHead(200, { 'Content-Type': 'text/plain' });
    *     response.end('Hello World\\n');
    *   }).listen(8124);
    *
    *   console.log('Server running at http://127.0.0.1:8124/');
    * })`;
    *
    * vm.runInThisContext(code)(require);
    * ```
    *
    * The `require()` in the above case shares the state with the context it is
    * passed from. This may introduce risks when untrusted code is executed, e.g.
    * altering objects in the context in unwanted ways.
    * @since v0.3.1
    * @param code The JavaScript code to compile and run.
    * @return the result of the very last statement executed in the script.
    */
  def runInThisContext(code: String): Any = js.native
  def runInThisContext(code: String, options: String): Any = js.native
  def runInThisContext(code: String, options: RunningCodeOptions): Any = js.native
}
