package typings.node

import typings.node.anon.Length
import typings.node.anon.PrivateKey
import typings.node.bufferMod.global.Buffer
import typings.node.bufferMod.global.BufferEncoding
import typings.node.cryptoMod.BinaryLike
import typings.node.cryptoMod.BinaryToTextEncoding
import typings.node.cryptoMod.CheckPrimeOptions
import typings.node.cryptoMod.CipherCCM
import typings.node.cryptoMod.CipherCCMOptions
import typings.node.cryptoMod.CipherCCMTypes
import typings.node.cryptoMod.CipherGCM
import typings.node.cryptoMod.CipherGCMOptions
import typings.node.cryptoMod.CipherGCMTypes
import typings.node.cryptoMod.CipherInfo
import typings.node.cryptoMod.CipherInfoOptions
import typings.node.cryptoMod.CipherKey
import typings.node.cryptoMod.CipherOCB
import typings.node.cryptoMod.CipherOCBOptions
import typings.node.cryptoMod.CipherOCBTypes
import typings.node.cryptoMod.DSAKeyPairKeyObjectOptions
import typings.node.cryptoMod.DSAKeyPairOptions
import typings.node.cryptoMod.DecipherCCM
import typings.node.cryptoMod.DecipherGCM
import typings.node.cryptoMod.DecipherOCB
import typings.node.cryptoMod.DiffieHellmanGroupConstructor
import typings.node.cryptoMod.ECKeyPairKeyObjectOptions
import typings.node.cryptoMod.ECKeyPairOptions
import typings.node.cryptoMod.ED25519KeyPairKeyObjectOptions
import typings.node.cryptoMod.ED25519KeyPairOptions
import typings.node.cryptoMod.ED448KeyPairKeyObjectOptions
import typings.node.cryptoMod.ED448KeyPairOptions
import typings.node.cryptoMod.GeneratePrimeOptions
import typings.node.cryptoMod.GeneratePrimeOptionsArrayBuffer
import typings.node.cryptoMod.GeneratePrimeOptionsBigInt
import typings.node.cryptoMod.HashOptions
import typings.node.cryptoMod.JsonWebKeyInput
import typings.node.cryptoMod.KeyLike
import typings.node.cryptoMod.KeyPairKeyObjectResult
import typings.node.cryptoMod.KeyPairSyncResult
import typings.node.cryptoMod.LargeNumberLike
import typings.node.cryptoMod.PrivateKeyInput
import typings.node.cryptoMod.PublicKeyInput
import typings.node.cryptoMod.RSAKeyPairKeyObjectOptions
import typings.node.cryptoMod.RSAKeyPairOptions
import typings.node.cryptoMod.RSAPSSKeyPairKeyObjectOptions
import typings.node.cryptoMod.RSAPSSKeyPairOptions
import typings.node.cryptoMod.RandomUUIDOptions
import typings.node.cryptoMod.RsaPrivateKey
import typings.node.cryptoMod.RsaPublicKey
import typings.node.cryptoMod.ScryptOptions
import typings.node.cryptoMod.SecureHeapUsage
import typings.node.cryptoMod.SignJsonWebKeyInput
import typings.node.cryptoMod.SignKeyObjectInput
import typings.node.cryptoMod.SignPrivateKeyInput
import typings.node.cryptoMod.UUID
import typings.node.cryptoMod.VerifyJsonWebKeyInput
import typings.node.cryptoMod.VerifyKeyObjectInput
import typings.node.cryptoMod.VerifyPublicKeyInput
import typings.node.cryptoMod.X25519KeyPairKeyObjectOptions
import typings.node.cryptoMod.X25519KeyPairOptions
import typings.node.cryptoMod.X448KeyPairKeyObjectOptions
import typings.node.cryptoMod.X448KeyPairOptions
import typings.node.cryptoMod.webcrypto.Crypto
import typings.node.cryptoMod.webcrypto.CryptoKey
import typings.node.cryptoMod.webcrypto.SubtleCrypto
import typings.node.globalsMod.global.NodeJS.ArrayBufferView
import typings.node.globalsMod.global.RelativeIndexable
import typings.node.nodeInts.`0`
import typings.node.nodeInts.`1`
import typings.node.nodeStrings.`rsa-pss`
import typings.node.nodeStrings.aes
import typings.node.nodeStrings.base64
import typings.node.nodeStrings.base64url
import typings.node.nodeStrings.buffer_
import typings.node.nodeStrings.compressed
import typings.node.nodeStrings.der
import typings.node.nodeStrings.dsa
import typings.node.nodeStrings.ec
import typings.node.nodeStrings.ed25519
import typings.node.nodeStrings.ed448
import typings.node.nodeStrings.hex
import typings.node.nodeStrings.hmac
import typings.node.nodeStrings.hybrid
import typings.node.nodeStrings.latin1
import typings.node.nodeStrings.pem
import typings.node.nodeStrings.rsa
import typings.node.nodeStrings.uncompressed
import typings.node.nodeStrings.x25519
import typings.node.nodeStrings.x448
import typings.node.streamMod.TransformOptions
import typings.node.streamMod.WritableOptions
import typings.node.workerThreadsMod._TransferListItem
import org.scalablytyped.runtime.StObject
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSGlobal, JSImport, JSName, JSBracketAccess}

object nodeColoncryptoMod {
  
  @JSImport("node:crypto", JSImport.Namespace)
  @js.native
  val ^ : js.Any = js.native
  
  /**
    * SPKAC is a Certificate Signing Request mechanism originally implemented by
    * Netscape and was specified formally as part of HTML5's `keygen` element.
    *
    * `<keygen>` is deprecated since [HTML 5.2](https://www.w3.org/TR/html52/changes.html#features-removed) and new projects
    * should not use this element anymore.
    *
    * The `node:crypto` module provides the `Certificate` class for working with SPKAC
    * data. The most common usage is handling output generated by the HTML5 `<keygen>` element. Node.js uses [OpenSSL's SPKAC
    * implementation](https://www.openssl.org/docs/man3.0/man1/openssl-spkac.html) internally.
    * @since v0.11.8
    */
  @JSImport("node:crypto", "Certificate")
  @js.native
  open class Certificate ()
    extends typings.node.cryptoMod.Certificate
  object Certificate {
    
    @JSImport("node:crypto", "Certificate")
    @js.native
    val ^ : js.Any = js.native
    
    /**
      * ```js
      * const { Certificate } = await import('node:crypto');
      * const spkac = getSpkacSomehow();
      * const challenge = Certificate.exportChallenge(spkac);
      * console.log(challenge.toString('utf8'));
      * // Prints: the challenge as a UTF8 string
      * ```
      * @since v9.0.0
      * @param encoding The `encoding` of the `spkac` string.
      * @return The challenge component of the `spkac` data structure, which includes a public key and a challenge.
      */
    /* static member */
    inline def exportChallenge(spkac: BinaryLike): Buffer = ^.asInstanceOf[js.Dynamic].applyDynamic("exportChallenge")(spkac.asInstanceOf[js.Any]).asInstanceOf[Buffer]
    
    /**
      * ```js
      * const { Certificate } = await import('node:crypto');
      * const spkac = getSpkacSomehow();
      * const publicKey = Certificate.exportPublicKey(spkac);
      * console.log(publicKey);
      * // Prints: the public key as <Buffer ...>
      * ```
      * @since v9.0.0
      * @param encoding The `encoding` of the `spkac` string.
      * @return The public key component of the `spkac` data structure, which includes a public key and a challenge.
      */
    /* static member */
    inline def exportPublicKey(spkac: BinaryLike): Buffer = ^.asInstanceOf[js.Dynamic].applyDynamic("exportPublicKey")(spkac.asInstanceOf[js.Any]).asInstanceOf[Buffer]
    inline def exportPublicKey(spkac: BinaryLike, encoding: String): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("exportPublicKey")(spkac.asInstanceOf[js.Any], encoding.asInstanceOf[js.Any])).asInstanceOf[Buffer]
    
    /**
      * ```js
      * import { Buffer } from 'node:buffer';
      * const { Certificate } = await import('node:crypto');
      *
      * const spkac = getSpkacSomehow();
      * console.log(Certificate.verifySpkac(Buffer.from(spkac)));
      * // Prints: true or false
      * ```
      * @since v9.0.0
      * @param encoding The `encoding` of the `spkac` string.
      * @return `true` if the given `spkac` data structure is valid, `false` otherwise.
      */
    /* static member */
    inline def verifySpkac(spkac: ArrayBufferView): Boolean = ^.asInstanceOf[js.Dynamic].applyDynamic("verifySpkac")(spkac.asInstanceOf[js.Any]).asInstanceOf[Boolean]
  }
  
  /**
    * Instances of the `Cipher` class are used to encrypt data. The class can be
    * used in one of two ways:
    *
    * * As a `stream` that is both readable and writable, where plain unencrypted
    * data is written to produce encrypted data on the readable side, or
    * * Using the `cipher.update()` and `cipher.final()` methods to produce
    * the encrypted data.
    *
    * The {@link createCipheriv} method is
    * used to create `Cipher` instances. `Cipher` objects are not to be created
    * directly using the `new` keyword.
    *
    * Example: Using `Cipher` objects as streams:
    *
    * ```js
    * const {
    *   scrypt,
    *   randomFill,
    *   createCipheriv,
    * } = await import('node:crypto');
    *
    * const algorithm = 'aes-192-cbc';
    * const password = 'Password used to generate key';
    *
    * // First, we'll generate the key. The key length is dependent on the algorithm.
    * // In this case for aes192, it is 24 bytes (192 bits).
    * scrypt(password, 'salt', 24, (err, key) => {
    *   if (err) throw err;
    *   // Then, we'll generate a random initialization vector
    *   randomFill(new Uint8Array(16), (err, iv) => {
    *     if (err) throw err;
    *
    *     // Once we have the key and iv, we can create and use the cipher...
    *     const cipher = createCipheriv(algorithm, key, iv);
    *
    *     let encrypted = '';
    *     cipher.setEncoding('hex');
    *
    *     cipher.on('data', (chunk) => encrypted += chunk);
    *     cipher.on('end', () => console.log(encrypted));
    *
    *     cipher.write('some clear text data');
    *     cipher.end();
    *   });
    * });
    * ```
    *
    * Example: Using `Cipher` and piped streams:
    *
    * ```js
    * import {
    *   createReadStream,
    *   createWriteStream,
    * } from 'node:fs';
    *
    * import {
    *   pipeline,
    * } from 'node:stream';
    *
    * const {
    *   scrypt,
    *   randomFill,
    *   createCipheriv,
    * } = await import('node:crypto');
    *
    * const algorithm = 'aes-192-cbc';
    * const password = 'Password used to generate key';
    *
    * // First, we'll generate the key. The key length is dependent on the algorithm.
    * // In this case for aes192, it is 24 bytes (192 bits).
    * scrypt(password, 'salt', 24, (err, key) => {
    *   if (err) throw err;
    *   // Then, we'll generate a random initialization vector
    *   randomFill(new Uint8Array(16), (err, iv) => {
    *     if (err) throw err;
    *
    *     const cipher = createCipheriv(algorithm, key, iv);
    *
    *     const input = createReadStream('test.js');
    *     const output = createWriteStream('test.enc');
    *
    *     pipeline(input, cipher, output, (err) => {
    *       if (err) throw err;
    *     });
    *   });
    * });
    * ```
    *
    * Example: Using the `cipher.update()` and `cipher.final()` methods:
    *
    * ```js
    * const {
    *   scrypt,
    *   randomFill,
    *   createCipheriv,
    * } = await import('node:crypto');
    *
    * const algorithm = 'aes-192-cbc';
    * const password = 'Password used to generate key';
    *
    * // First, we'll generate the key. The key length is dependent on the algorithm.
    * // In this case for aes192, it is 24 bytes (192 bits).
    * scrypt(password, 'salt', 24, (err, key) => {
    *   if (err) throw err;
    *   // Then, we'll generate a random initialization vector
    *   randomFill(new Uint8Array(16), (err, iv) => {
    *     if (err) throw err;
    *
    *     const cipher = createCipheriv(algorithm, key, iv);
    *
    *     let encrypted = cipher.update('some clear text data', 'utf8', 'hex');
    *     encrypted += cipher.final('hex');
    *     console.log(encrypted);
    *   });
    * });
    * ```
    * @since v0.1.94
    */
  @JSImport("node:crypto", "Cipher")
  @js.native
  /* private */ open class Cipher () extends StObject
  
  /**
    * Instances of the `Decipher` class are used to decrypt data. The class can be
    * used in one of two ways:
    *
    * * As a `stream` that is both readable and writable, where plain encrypted
    * data is written to produce unencrypted data on the readable side, or
    * * Using the `decipher.update()` and `decipher.final()` methods to
    * produce the unencrypted data.
    *
    * The {@link createDecipheriv} method is
    * used to create `Decipher` instances. `Decipher` objects are not to be created
    * directly using the `new` keyword.
    *
    * Example: Using `Decipher` objects as streams:
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const {
    *   scryptSync,
    *   createDecipheriv,
    * } = await import('node:crypto');
    *
    * const algorithm = 'aes-192-cbc';
    * const password = 'Password used to generate key';
    * // Key length is dependent on the algorithm. In this case for aes192, it is
    * // 24 bytes (192 bits).
    * // Use the async `crypto.scrypt()` instead.
    * const key = scryptSync(password, 'salt', 24);
    * // The IV is usually passed along with the ciphertext.
    * const iv = Buffer.alloc(16, 0); // Initialization vector.
    *
    * const decipher = createDecipheriv(algorithm, key, iv);
    *
    * let decrypted = '';
    * decipher.on('readable', () => {
    *   let chunk;
    *   while (null !== (chunk = decipher.read())) {
    *     decrypted += chunk.toString('utf8');
    *   }
    * });
    * decipher.on('end', () => {
    *   console.log(decrypted);
    *   // Prints: some clear text data
    * });
    *
    * // Encrypted with same algorithm, key and iv.
    * const encrypted =
    *   'e5f79c5915c02171eec6b212d5520d44480993d7d622a7c4c2da32f6efda0ffa';
    * decipher.write(encrypted, 'hex');
    * decipher.end();
    * ```
    *
    * Example: Using `Decipher` and piped streams:
    *
    * ```js
    * import {
    *   createReadStream,
    *   createWriteStream,
    * } from 'node:fs';
    * import { Buffer } from 'node:buffer';
    * const {
    *   scryptSync,
    *   createDecipheriv,
    * } = await import('node:crypto');
    *
    * const algorithm = 'aes-192-cbc';
    * const password = 'Password used to generate key';
    * // Use the async `crypto.scrypt()` instead.
    * const key = scryptSync(password, 'salt', 24);
    * // The IV is usually passed along with the ciphertext.
    * const iv = Buffer.alloc(16, 0); // Initialization vector.
    *
    * const decipher = createDecipheriv(algorithm, key, iv);
    *
    * const input = createReadStream('test.enc');
    * const output = createWriteStream('test.js');
    *
    * input.pipe(decipher).pipe(output);
    * ```
    *
    * Example: Using the `decipher.update()` and `decipher.final()` methods:
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const {
    *   scryptSync,
    *   createDecipheriv,
    * } = await import('node:crypto');
    *
    * const algorithm = 'aes-192-cbc';
    * const password = 'Password used to generate key';
    * // Use the async `crypto.scrypt()` instead.
    * const key = scryptSync(password, 'salt', 24);
    * // The IV is usually passed along with the ciphertext.
    * const iv = Buffer.alloc(16, 0); // Initialization vector.
    *
    * const decipher = createDecipheriv(algorithm, key, iv);
    *
    * // Encrypted using same algorithm, key and iv.
    * const encrypted =
    *   'e5f79c5915c02171eec6b212d5520d44480993d7d622a7c4c2da32f6efda0ffa';
    * let decrypted = decipher.update(encrypted, 'hex', 'utf8');
    * decrypted += decipher.final('utf8');
    * console.log(decrypted);
    * // Prints: some clear text data
    * ```
    * @since v0.1.94
    */
  @JSImport("node:crypto", "Decipher")
  @js.native
  /* private */ open class Decipher () extends StObject
  
  /**
    * The `DiffieHellmanGroup` class takes a well-known modp group as its argument.
    * It works the same as `DiffieHellman`, except that it does not allow changing its keys after creation.
    * In other words, it does not implement `setPublicKey()` or `setPrivateKey()` methods.
    *
    * ```js
    * const { createDiffieHellmanGroup } = await import('node:crypto');
    * const dh = createDiffieHellmanGroup('modp1');
    * ```
    * The name (e.g. `'modp1'`) is taken from [RFC 2412](https://www.rfc-editor.org/rfc/rfc2412.txt) (modp1 and 2) and [RFC 3526](https://www.rfc-editor.org/rfc/rfc3526.txt):
    * ```bash
    * $ perl -ne 'print "$1\n" if /"(modp\d+)"/' src/node_crypto_groups.h
    * modp1  #  768 bits
    * modp2  # 1024 bits
    * modp5  # 1536 bits
    * modp14 # 2048 bits
    * modp15 # etc.
    * modp16
    * modp17
    * modp18
    * ```
    * @since v0.7.5
    */
  @JSImport("node:crypto", "DiffieHellmanGroup")
  @js.native
  val DiffieHellmanGroup: DiffieHellmanGroupConstructor = js.native
  
  /* This class was inferred from a value with a constructor, it was renamed because a distinct type already exists with the same name. */
  @JSImport("node:crypto", "DiffieHellmanGroup")
  @js.native
  open class DiffieHellmanGroupCls protected ()
    extends StObject
       with typings.node.cryptoMod.DiffieHellmanGroup {
    def this(name: String) = this()
    
    /* CompleteClass */
    override def computeSecret(otherPublicKey: ArrayBufferView): Buffer = js.native
    /* CompleteClass */
    @JSName("computeSecret")
    var computeSecret_Original: js.Function3[
        /* otherPublicKey */ ArrayBufferView, 
        /* inputEncoding */ js.UndefOr[Null], 
        /* outputEncoding */ js.UndefOr[Null], 
        Buffer
      ] = js.native
    
    /* CompleteClass */
    @JSName("constructor")
    var constructor_Original: js.Function0[Any] = js.native
    
    /* CompleteClass */
    override def generateKeys(): Buffer = js.native
    /* CompleteClass */
    @JSName("generateKeys")
    var generateKeys_Original: js.Function0[Buffer] = js.native
    
    /* CompleteClass */
    override def getGenerator(): Buffer = js.native
    /* CompleteClass */
    @JSName("getGenerator")
    var getGenerator_Original: js.Function0[Buffer] = js.native
    
    /* CompleteClass */
    override def getPrime(): Buffer = js.native
    /* CompleteClass */
    @JSName("getPrime")
    var getPrime_Original: js.Function0[Buffer] = js.native
    
    /* CompleteClass */
    override def getPrivateKey(): Buffer = js.native
    /* CompleteClass */
    @JSName("getPrivateKey")
    var getPrivateKey_Original: js.Function0[Buffer] = js.native
    
    /* CompleteClass */
    override def getPublicKey(): Buffer = js.native
    /* CompleteClass */
    @JSName("getPublicKey")
    var getPublicKey_Original: js.Function0[Buffer] = js.native
    
    /* CompleteClass */
    var verifyError: Double = js.native
  }
  
  /**
    * The `DiffieHellman` class is a utility for creating Diffie-Hellman key
    * exchanges.
    *
    * Instances of the `DiffieHellman` class can be created using the {@link createDiffieHellman} function.
    *
    * ```js
    * import assert from 'node:assert';
    *
    * const {
    *   createDiffieHellman,
    * } = await import('node:crypto');
    *
    * // Generate Alice's keys...
    * const alice = createDiffieHellman(2048);
    * const aliceKey = alice.generateKeys();
    *
    * // Generate Bob's keys...
    * const bob = createDiffieHellman(alice.getPrime(), alice.getGenerator());
    * const bobKey = bob.generateKeys();
    *
    * // Exchange and generate the secret...
    * const aliceSecret = alice.computeSecret(bobKey);
    * const bobSecret = bob.computeSecret(aliceKey);
    *
    * // OK
    * assert.strictEqual(aliceSecret.toString('hex'), bobSecret.toString('hex'));
    * ```
    * @since v0.5.0
    */
  @JSImport("node:crypto", "DiffieHellman")
  @js.native
  /* private */ open class DiffieHellman_ ()
    extends typings.node.cryptoMod.DiffieHellman_
  
  /**
    * The `ECDH` class is a utility for creating Elliptic Curve Diffie-Hellman (ECDH)
    * key exchanges.
    *
    * Instances of the `ECDH` class can be created using the {@link createECDH} function.
    *
    * ```js
    * import assert from 'node:assert';
    *
    * const {
    *   createECDH,
    * } = await import('node:crypto');
    *
    * // Generate Alice's keys...
    * const alice = createECDH('secp521r1');
    * const aliceKey = alice.generateKeys();
    *
    * // Generate Bob's keys...
    * const bob = createECDH('secp521r1');
    * const bobKey = bob.generateKeys();
    *
    * // Exchange and generate the secret...
    * const aliceSecret = alice.computeSecret(bobKey);
    * const bobSecret = bob.computeSecret(aliceKey);
    *
    * assert.strictEqual(aliceSecret.toString('hex'), bobSecret.toString('hex'));
    * // OK
    * ```
    * @since v0.11.14
    */
  @JSImport("node:crypto", "ECDH")
  @js.native
  /* private */ open class ECDH ()
    extends typings.node.cryptoMod.ECDH
  object ECDH {
    
    @JSImport("node:crypto", "ECDH")
    @js.native
    val ^ : js.Any = js.native
    
    /**
      * Converts the EC Diffie-Hellman public key specified by `key` and `curve` to the
      * format specified by `format`. The `format` argument specifies point encoding
      * and can be `'compressed'`, `'uncompressed'` or `'hybrid'`. The supplied key is
      * interpreted using the specified `inputEncoding`, and the returned key is encoded
      * using the specified `outputEncoding`.
      *
      * Use {@link getCurves} to obtain a list of available curve names.
      * On recent OpenSSL releases, `openssl ecparam -list_curves` will also display
      * the name and description of each available elliptic curve.
      *
      * If `format` is not specified the point will be returned in `'uncompressed'` format.
      *
      * If the `inputEncoding` is not provided, `key` is expected to be a `Buffer`, `TypedArray`, or `DataView`.
      *
      * Example (uncompressing a key):
      *
      * ```js
      * const {
      *   createECDH,
      *   ECDH,
      * } = await import('node:crypto');
      *
      * const ecdh = createECDH('secp256k1');
      * ecdh.generateKeys();
      *
      * const compressedKey = ecdh.getPublicKey('hex', 'compressed');
      *
      * const uncompressedKey = ECDH.convertKey(compressedKey,
      *                                         'secp256k1',
      *                                         'hex',
      *                                         'hex',
      *                                         'uncompressed');
      *
      * // The converted key and the uncompressed public key should be the same
      * console.log(uncompressedKey === ecdh.getPublicKey('hex'));
      * ```
      * @since v10.0.0
      * @param inputEncoding The `encoding` of the `key` string.
      * @param outputEncoding The `encoding` of the return value.
      * @param [format='uncompressed']
      */
    /* static member */
    inline def convertKey(key: BinaryLike, curve: String): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(
      key: BinaryLike,
      curve: String,
      inputEncoding: Unit,
      outputEncoding: latin1 | hex | base64 | base64url
    ): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(
      key: BinaryLike,
      curve: String,
      inputEncoding: Unit,
      outputEncoding: latin1 | hex | base64 | base64url,
      format: uncompressed | compressed | hybrid
    ): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any], format.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(
      key: BinaryLike,
      curve: String,
      inputEncoding: Unit,
      outputEncoding: Unit,
      format: uncompressed | compressed | hybrid
    ): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any], format.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(key: BinaryLike, curve: String, inputEncoding: BinaryToTextEncoding): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(
      key: BinaryLike,
      curve: String,
      inputEncoding: BinaryToTextEncoding,
      outputEncoding: latin1 | hex | base64 | base64url
    ): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(
      key: BinaryLike,
      curve: String,
      inputEncoding: BinaryToTextEncoding,
      outputEncoding: latin1 | hex | base64 | base64url,
      format: uncompressed | compressed | hybrid
    ): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any], format.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
    inline def convertKey(
      key: BinaryLike,
      curve: String,
      inputEncoding: BinaryToTextEncoding,
      outputEncoding: Unit,
      format: uncompressed | compressed | hybrid
    ): Buffer | String = (^.asInstanceOf[js.Dynamic].applyDynamic("convertKey")(key.asInstanceOf[js.Any], curve.asInstanceOf[js.Any], inputEncoding.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any], format.asInstanceOf[js.Any])).asInstanceOf[Buffer | String]
  }
  
  /**
    * The `Hash` class is a utility for creating hash digests of data. It can be
    * used in one of two ways:
    *
    * * As a `stream` that is both readable and writable, where data is written
    * to produce a computed hash digest on the readable side, or
    * * Using the `hash.update()` and `hash.digest()` methods to produce the
    * computed hash.
    *
    * The {@link createHash} method is used to create `Hash` instances. `Hash`objects are not to be created directly using the `new` keyword.
    *
    * Example: Using `Hash` objects as streams:
    *
    * ```js
    * const {
    *   createHash,
    * } = await import('node:crypto');
    *
    * const hash = createHash('sha256');
    *
    * hash.on('readable', () => {
    *   // Only one element is going to be produced by the
    *   // hash stream.
    *   const data = hash.read();
    *   if (data) {
    *     console.log(data.toString('hex'));
    *     // Prints:
    *     //   6a2da20943931e9834fc12cfe5bb47bbd9ae43489a30726962b576f4e3993e50
    *   }
    * });
    *
    * hash.write('some data to hash');
    * hash.end();
    * ```
    *
    * Example: Using `Hash` and piped streams:
    *
    * ```js
    * import { createReadStream } from 'node:fs';
    * import { stdout } from 'node:process';
    * const { createHash } = await import('node:crypto');
    *
    * const hash = createHash('sha256');
    *
    * const input = createReadStream('test.js');
    * input.pipe(hash).setEncoding('hex').pipe(stdout);
    * ```
    *
    * Example: Using the `hash.update()` and `hash.digest()` methods:
    *
    * ```js
    * const {
    *   createHash,
    * } = await import('node:crypto');
    *
    * const hash = createHash('sha256');
    *
    * hash.update('some data to hash');
    * console.log(hash.digest('hex'));
    * // Prints:
    * //   6a2da20943931e9834fc12cfe5bb47bbd9ae43489a30726962b576f4e3993e50
    * ```
    * @since v0.1.92
    */
  @JSImport("node:crypto", "Hash")
  @js.native
  /* private */ open class Hash_ () extends StObject
  
  /**
    * The `Hmac` class is a utility for creating cryptographic HMAC digests. It can
    * be used in one of two ways:
    *
    * * As a `stream` that is both readable and writable, where data is written
    * to produce a computed HMAC digest on the readable side, or
    * * Using the `hmac.update()` and `hmac.digest()` methods to produce the
    * computed HMAC digest.
    *
    * The {@link createHmac} method is used to create `Hmac` instances. `Hmac`objects are not to be created directly using the `new` keyword.
    *
    * Example: Using `Hmac` objects as streams:
    *
    * ```js
    * const {
    *   createHmac,
    * } = await import('node:crypto');
    *
    * const hmac = createHmac('sha256', 'a secret');
    *
    * hmac.on('readable', () => {
    *   // Only one element is going to be produced by the
    *   // hash stream.
    *   const data = hmac.read();
    *   if (data) {
    *     console.log(data.toString('hex'));
    *     // Prints:
    *     //   7fd04df92f636fd450bc841c9418e5825c17f33ad9c87c518115a45971f7f77e
    *   }
    * });
    *
    * hmac.write('some data to hash');
    * hmac.end();
    * ```
    *
    * Example: Using `Hmac` and piped streams:
    *
    * ```js
    * import { createReadStream } from 'node:fs';
    * import { stdout } from 'node:process';
    * const {
    *   createHmac,
    * } = await import('node:crypto');
    *
    * const hmac = createHmac('sha256', 'a secret');
    *
    * const input = createReadStream('test.js');
    * input.pipe(hmac).pipe(stdout);
    * ```
    *
    * Example: Using the `hmac.update()` and `hmac.digest()` methods:
    *
    * ```js
    * const {
    *   createHmac,
    * } = await import('node:crypto');
    *
    * const hmac = createHmac('sha256', 'a secret');
    *
    * hmac.update('some data to hash');
    * console.log(hmac.digest('hex'));
    * // Prints:
    * //   7fd04df92f636fd450bc841c9418e5825c17f33ad9c87c518115a45971f7f77e
    * ```
    * @since v0.1.94
    * @deprecated Since v20.13.0 Calling `Hmac` class directly with `Hmac()` or `new Hmac()` is deprecated due to being internals, not intended for public use. Please use the {@link createHmac} method to create Hmac instances.
    */
  @JSImport("node:crypto", "Hmac")
  @js.native
  /* private */ open class Hmac () extends StObject
  
  /**
    * Node.js uses a `KeyObject` class to represent a symmetric or asymmetric key,
    * and each kind of key exposes different functions. The {@link createSecretKey}, {@link createPublicKey} and {@link createPrivateKey} methods are used to create `KeyObject`instances. `KeyObject`
    * objects are not to be created directly using the `new`keyword.
    *
    * Most applications should consider using the new `KeyObject` API instead of
    * passing keys as strings or `Buffer`s due to improved security features.
    *
    * `KeyObject` instances can be passed to other threads via `postMessage()`.
    * The receiver obtains a cloned `KeyObject`, and the `KeyObject` does not need to
    * be listed in the `transferList` argument.
    * @since v11.6.0
    */
  @JSImport("node:crypto", "KeyObject")
  @js.native
  /* private */ open class KeyObject ()
    extends typings.node.cryptoMod.KeyObject
  object KeyObject {
    
    @JSImport("node:crypto", "KeyObject")
    @js.native
    val ^ : js.Any = js.native
    
    /**
      * Example: Converting a `CryptoKey` instance to a `KeyObject`:
      *
      * ```js
      * const { KeyObject } = await import('node:crypto');
      * const { subtle } = globalThis.crypto;
      *
      * const key = await subtle.generateKey({
      *   name: 'HMAC',
      *   hash: 'SHA-256',
      *   length: 256,
      * }, true, ['sign', 'verify']);
      *
      * const keyObject = KeyObject.from(key);
      * console.log(keyObject.symmetricKeySize);
      * // Prints: 32 (symmetric key size in bytes)
      * ```
      * @since v15.0.0
      */
    /* static member */
    inline def from(key: CryptoKey): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("from")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  }
  
  /**
    * The `Sign` class is a utility for generating signatures. It can be used in one
    * of two ways:
    *
    * * As a writable `stream`, where data to be signed is written and the `sign.sign()` method is used to generate and return the signature, or
    * * Using the `sign.update()` and `sign.sign()` methods to produce the
    * signature.
    *
    * The {@link createSign} method is used to create `Sign` instances. The
    * argument is the string name of the hash function to use. `Sign` objects are not
    * to be created directly using the `new` keyword.
    *
    * Example: Using `Sign` and `Verify` objects as streams:
    *
    * ```js
    * const {
    *   generateKeyPairSync,
    *   createSign,
    *   createVerify,
    * } = await import('node:crypto');
    *
    * const { privateKey, publicKey } = generateKeyPairSync('ec', {
    *   namedCurve: 'sect239k1',
    * });
    *
    * const sign = createSign('SHA256');
    * sign.write('some data to sign');
    * sign.end();
    * const signature = sign.sign(privateKey, 'hex');
    *
    * const verify = createVerify('SHA256');
    * verify.write('some data to sign');
    * verify.end();
    * console.log(verify.verify(publicKey, signature, 'hex'));
    * // Prints: true
    * ```
    *
    * Example: Using the `sign.update()` and `verify.update()` methods:
    *
    * ```js
    * const {
    *   generateKeyPairSync,
    *   createSign,
    *   createVerify,
    * } = await import('node:crypto');
    *
    * const { privateKey, publicKey } = generateKeyPairSync('rsa', {
    *   modulusLength: 2048,
    * });
    *
    * const sign = createSign('SHA256');
    * sign.update('some data to sign');
    * sign.end();
    * const signature = sign.sign(privateKey);
    *
    * const verify = createVerify('SHA256');
    * verify.update('some data to sign');
    * verify.end();
    * console.log(verify.verify(publicKey, signature));
    * // Prints: true
    * ```
    * @since v0.1.92
    */
  @JSImport("node:crypto", "Sign")
  @js.native
  /* private */ open class Sign_ () extends StObject
  
  /**
    * The `Verify` class is a utility for verifying signatures. It can be used in one
    * of two ways:
    *
    * * As a writable `stream` where written data is used to validate against the
    * supplied signature, or
    * * Using the `verify.update()` and `verify.verify()` methods to verify
    * the signature.
    *
    * The {@link createVerify} method is used to create `Verify` instances. `Verify` objects are not to be created directly using the `new` keyword.
    *
    * See `Sign` for examples.
    * @since v0.1.92
    */
  @JSImport("node:crypto", "Verify")
  @js.native
  /* private */ open class Verify_ () extends StObject
  
  /**
    * Encapsulates an X509 certificate and provides read-only access to
    * its information.
    *
    * ```js
    * const { X509Certificate } = await import('node:crypto');
    *
    * const x509 = new X509Certificate('{... pem encoded cert ...}');
    *
    * console.log(x509.subject);
    * ```
    * @since v15.6.0
    */
  @JSImport("node:crypto", "X509Certificate")
  @js.native
  open class X509Certificate protected ()
    extends typings.node.cryptoMod.X509Certificate
       with _TransferListItem {
    def this(buffer: BinaryLike) = this()
  }
  
  /**
    * Checks the primality of the `candidate`.
    * @since v15.8.0
    * @param candidate A possible prime encoded as a sequence of big endian octets of arbitrary length.
    */
  inline def checkPrime(
    value: LargeNumberLike,
    callback: js.Function2[/* err */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("checkPrime")(value.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def checkPrime(
    value: LargeNumberLike,
    options: CheckPrimeOptions,
    callback: js.Function2[/* err */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("checkPrime")(value.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Checks the primality of the `candidate`.
    * @since v15.8.0
    * @param candidate A possible prime encoded as a sequence of big endian octets of arbitrary length.
    * @return `true` if the candidate is a prime with an error probability less than `0.25 ** options.checks`.
    */
  inline def checkPrimeSync(candidate: LargeNumberLike): Boolean = ^.asInstanceOf[js.Dynamic].applyDynamic("checkPrimeSync")(candidate.asInstanceOf[js.Any]).asInstanceOf[Boolean]
  inline def checkPrimeSync(candidate: LargeNumberLike, options: CheckPrimeOptions): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("checkPrimeSync")(candidate.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  
  object constants {
    
    @JSImport("node:crypto", "constants.DH_CHECK_P_NOT_PRIME")
    @js.native
    val DH_CHECK_P_NOT_PRIME: Double = js.native
    
    @JSImport("node:crypto", "constants.DH_CHECK_P_NOT_SAFE_PRIME")
    @js.native
    val DH_CHECK_P_NOT_SAFE_PRIME: Double = js.native
    
    @JSImport("node:crypto", "constants.DH_NOT_SUITABLE_GENERATOR")
    @js.native
    val DH_NOT_SUITABLE_GENERATOR: Double = js.native
    
    @JSImport("node:crypto", "constants.DH_UNABLE_TO_CHECK_GENERATOR")
    @js.native
    val DH_UNABLE_TO_CHECK_GENERATOR: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_ALL")
    @js.native
    val ENGINE_METHOD_ALL: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_CIPHERS")
    @js.native
    val ENGINE_METHOD_CIPHERS: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_DH")
    @js.native
    val ENGINE_METHOD_DH: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_DIGESTS")
    @js.native
    val ENGINE_METHOD_DIGESTS: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_DSA")
    @js.native
    val ENGINE_METHOD_DSA: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_EC")
    @js.native
    val ENGINE_METHOD_EC: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_NONE")
    @js.native
    val ENGINE_METHOD_NONE: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_PKEY_ASN1_METHS")
    @js.native
    val ENGINE_METHOD_PKEY_ASN1_METHS: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_PKEY_METHS")
    @js.native
    val ENGINE_METHOD_PKEY_METHS: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_RAND")
    @js.native
    val ENGINE_METHOD_RAND: Double = js.native
    
    @JSImport("node:crypto", "constants.ENGINE_METHOD_RSA")
    @js.native
    val ENGINE_METHOD_RSA: Double = js.native
    
    // https://nodejs.org/dist/latest-v22.x/docs/api/crypto.html#crypto-constants
    @JSImport("node:crypto", "constants.OPENSSL_VERSION_NUMBER")
    @js.native
    val OPENSSL_VERSION_NUMBER: Double = js.native
    
    @JSImport("node:crypto", "constants.POINT_CONVERSION_COMPRESSED")
    @js.native
    val POINT_CONVERSION_COMPRESSED: Double = js.native
    
    @JSImport("node:crypto", "constants.POINT_CONVERSION_HYBRID")
    @js.native
    val POINT_CONVERSION_HYBRID: Double = js.native
    
    @JSImport("node:crypto", "constants.POINT_CONVERSION_UNCOMPRESSED")
    @js.native
    val POINT_CONVERSION_UNCOMPRESSED: Double = js.native
    
    @JSImport("node:crypto", "constants.RSA_NO_PADDING")
    @js.native
    val RSA_NO_PADDING: Double = js.native
    
    @JSImport("node:crypto", "constants.RSA_PKCS1_OAEP_PADDING")
    @js.native
    val RSA_PKCS1_OAEP_PADDING: Double = js.native
    
    @JSImport("node:crypto", "constants.RSA_PKCS1_PADDING")
    @js.native
    val RSA_PKCS1_PADDING: Double = js.native
    
    @JSImport("node:crypto", "constants.RSA_PKCS1_PSS_PADDING")
    @js.native
    val RSA_PKCS1_PSS_PADDING: Double = js.native
    
    /** Causes the salt length for RSA_PKCS1_PSS_PADDING to be determined automatically when verifying a signature. */
    @JSImport("node:crypto", "constants.RSA_PSS_SALTLEN_AUTO")
    @js.native
    val RSA_PSS_SALTLEN_AUTO: Double = js.native
    
    /** Sets the salt length for RSA_PKCS1_PSS_PADDING to the digest size when signing or verifying. */
    @JSImport("node:crypto", "constants.RSA_PSS_SALTLEN_DIGEST")
    @js.native
    val RSA_PSS_SALTLEN_DIGEST: Double = js.native
    
    /** Sets the salt length for RSA_PKCS1_PSS_PADDING to the maximum permissible value when signing data. */
    @JSImport("node:crypto", "constants.RSA_PSS_SALTLEN_MAX_SIGN")
    @js.native
    val RSA_PSS_SALTLEN_MAX_SIGN: Double = js.native
    
    @JSImport("node:crypto", "constants.RSA_SSLV23_PADDING")
    @js.native
    val RSA_SSLV23_PADDING: Double = js.native
    
    @JSImport("node:crypto", "constants.RSA_X931_PADDING")
    @js.native
    val RSA_X931_PADDING: Double = js.native
    
    /** Instructs OpenSSL to turn off SSL v2 */
    @JSImport("node:crypto", "constants.SSL_OP_NO_SSLv2")
    @js.native
    val SSLOPNOSSLv2: Double = js.native
    
    /** Instructs OpenSSL to turn off SSL v3 */
    @JSImport("node:crypto", "constants.SSL_OP_NO_SSLv3")
    @js.native
    val SSLOPNOSSLv3: Double = js.native
    
    /** Instructs OpenSSL to turn off TLS v1 */
    @JSImport("node:crypto", "constants.SSL_OP_NO_TLSv1")
    @js.native
    val SSLOPNOTLSv1: Double = js.native
    
    /** Instructs OpenSSL to turn off TLS v1.1 */
    @JSImport("node:crypto", "constants.SSL_OP_NO_TLSv1_1")
    @js.native
    val SSLOPNOTLSv11: Double = js.native
    
    /** Instructs OpenSSL to turn off TLS v1.2 */
    @JSImport("node:crypto", "constants.SSL_OP_NO_TLSv1_2")
    @js.native
    val SSLOPNOTLSv12: Double = js.native
    
    /** Instructs OpenSSL to turn off TLS v1.3 */
    @JSImport("node:crypto", "constants.SSL_OP_NO_TLSv1_3")
    @js.native
    val SSLOPNOTLSv13: Double = js.native
    
    /** Applies multiple bug workarounds within OpenSSL. See https://www.openssl.org/docs/man1.0.2/ssl/SSL_CTX_set_options.html for detail. */
    @JSImport("node:crypto", "constants.SSL_OP_ALL")
    @js.native
    val SSL_OP_ALL: Double = js.native
    
    /** Instructs OpenSSL to allow a non-[EC]DHE-based key exchange mode for TLS v1.3 */
    @JSImport("node:crypto", "constants.SSL_OP_ALLOW_NO_DHE_KEX")
    @js.native
    val SSL_OP_ALLOW_NO_DHE_KEX: Double = js.native
    
    /** Allows legacy insecure renegotiation between OpenSSL and unpatched clients or servers. See https://www.openssl.org/docs/man1.0.2/ssl/SSL_CTX_set_options.html. */
    @JSImport("node:crypto", "constants.SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION")
    @js.native
    val SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION: Double = js.native
    
    /** Attempts to use the server's preferences instead of the client's when selecting a cipher. See https://www.openssl.org/docs/man1.0.2/ssl/SSL_CTX_set_options.html. */
    @JSImport("node:crypto", "constants.SSL_OP_CIPHER_SERVER_PREFERENCE")
    @js.native
    val SSL_OP_CIPHER_SERVER_PREFERENCE: Double = js.native
    
    /** Instructs OpenSSL to use Cisco's version identifier of DTLS_BAD_VER. */
    @JSImport("node:crypto", "constants.SSL_OP_CISCO_ANYCONNECT")
    @js.native
    val SSL_OP_CISCO_ANYCONNECT: Double = js.native
    
    /** Instructs OpenSSL to turn on cookie exchange. */
    @JSImport("node:crypto", "constants.SSL_OP_COOKIE_EXCHANGE")
    @js.native
    val SSL_OP_COOKIE_EXCHANGE: Double = js.native
    
    /** Instructs OpenSSL to add server-hello extension from an early version of the cryptopro draft. */
    @JSImport("node:crypto", "constants.SSL_OP_CRYPTOPRO_TLSEXT_BUG")
    @js.native
    val SSL_OP_CRYPTOPRO_TLSEXT_BUG: Double = js.native
    
    /** Instructs OpenSSL to disable a SSL 3.0/TLS 1.0 vulnerability workaround added in OpenSSL 0.9.6d. */
    @JSImport("node:crypto", "constants.SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS")
    @js.native
    val SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS: Double = js.native
    
    /** Allows initial connection to servers that do not support RI. */
    @JSImport("node:crypto", "constants.SSL_OP_LEGACY_SERVER_CONNECT")
    @js.native
    val SSL_OP_LEGACY_SERVER_CONNECT: Double = js.native
    
    /** Instructs OpenSSL to disable support for SSL/TLS compression. */
    @JSImport("node:crypto", "constants.SSL_OP_NO_COMPRESSION")
    @js.native
    val SSL_OP_NO_COMPRESSION: Double = js.native
    
    /** Instructs OpenSSL to disable encrypt-then-MAC. */
    @JSImport("node:crypto", "constants.SSL_OP_NO_ENCRYPT_THEN_MAC")
    @js.native
    val SSL_OP_NO_ENCRYPT_THEN_MAC: Double = js.native
    
    @JSImport("node:crypto", "constants.SSL_OP_NO_QUERY_MTU")
    @js.native
    val SSL_OP_NO_QUERY_MTU: Double = js.native
    
    /** Instructs OpenSSL to disable renegotiation. */
    @JSImport("node:crypto", "constants.SSL_OP_NO_RENEGOTIATION")
    @js.native
    val SSL_OP_NO_RENEGOTIATION: Double = js.native
    
    /** Instructs OpenSSL to always start a new session when performing renegotiation. */
    @JSImport("node:crypto", "constants.SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION")
    @js.native
    val SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION: Double = js.native
    
    /** Instructs OpenSSL to disable use of RFC4507bis tickets. */
    @JSImport("node:crypto", "constants.SSL_OP_NO_TICKET")
    @js.native
    val SSL_OP_NO_TICKET: Double = js.native
    
    /** Instructs OpenSSL server to prioritize ChaCha20-Poly1305 when the client does. This option has no effect if `SSL_OP_CIPHER_SERVER_PREFERENCE` is not enabled. */
    @JSImport("node:crypto", "constants.SSL_OP_PRIORITIZE_CHACHA")
    @js.native
    val SSL_OP_PRIORITIZE_CHACHA: Double = js.native
    
    /** Instructs OpenSSL to disable version rollback attack detection. */
    @JSImport("node:crypto", "constants.SSL_OP_TLS_ROLLBACK_BUG")
    @js.native
    val SSL_OP_TLS_ROLLBACK_BUG: Double = js.native
    
    /** Specifies the active default cipher list used by the current Node.js process  (colon-separated values). */
    @JSImport("node:crypto", "constants.defaultCipherList")
    @js.native
    val defaultCipherList: String = js.native
    
    /** Specifies the built-in default cipher list used by Node.js (colon-separated values). */
    @JSImport("node:crypto", "constants.defaultCoreCipherList")
    @js.native
    val defaultCoreCipherList: String = js.native
  }
  
  inline def createCipheriv(algorithm: String, key: CipherKey): typings.node.cryptoMod.Cipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Cipher]
  inline def createCipheriv(algorithm: String, key: CipherKey, iv: Null, options: TransformOptions): typings.node.cryptoMod.Cipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Cipher]
  inline def createCipheriv(algorithm: String, key: CipherKey, iv: BinaryLike): typings.node.cryptoMod.Cipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Cipher]
  inline def createCipheriv(algorithm: String, key: CipherKey, iv: BinaryLike, options: TransformOptions): typings.node.cryptoMod.Cipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Cipher]
  /**
    * Creates and returns a `Cipher` object, with the given `algorithm`, `key` and
    * initialization vector (`iv`).
    *
    * The `options` argument controls stream behavior and is optional except when a
    * cipher in CCM or OCB mode (e.g. `'aes-128-ccm'`) is used. In that case, the`authTagLength` option is required and specifies the length of the
    * authentication tag in bytes, see `CCM mode`. In GCM mode, the `authTagLength`option is not required but can be used to set the length of the authentication
    * tag that will be returned by `getAuthTag()` and defaults to 16 bytes.
    * For `chacha20-poly1305`, the `authTagLength` option defaults to 16 bytes.
    *
    * The `algorithm` is dependent on OpenSSL, examples are `'aes192'`, etc. On
    * recent OpenSSL releases, `openssl list -cipher-algorithms` will
    * display the available cipher algorithms.
    *
    * The `key` is the raw key used by the `algorithm` and `iv` is an [initialization vector](https://en.wikipedia.org/wiki/Initialization_vector). Both arguments must be `'utf8'` encoded
    * strings,`Buffers`, `TypedArray`, or `DataView`s. The `key` may optionally be
    * a `KeyObject` of type `secret`. If the cipher does not need
    * an initialization vector, `iv` may be `null`.
    *
    * When passing strings for `key` or `iv`, please consider `caveats when using strings as inputs to cryptographic APIs`.
    *
    * Initialization vectors should be unpredictable and unique; ideally, they will be
    * cryptographically random. They do not have to be secret: IVs are typically just
    * added to ciphertext messages unencrypted. It may sound contradictory that
    * something has to be unpredictable and unique, but does not have to be secret;
    * remember that an attacker must not be able to predict ahead of time what a
    * given IV will be.
    * @since v0.1.94
    * @param options `stream.transform` options
    */
  inline def createCipheriv(algorithm: CipherCCMTypes, key: CipherKey, iv: BinaryLike, options: CipherCCMOptions): CipherCCM = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[CipherCCM]
  inline def createCipheriv(algorithm: CipherGCMTypes, key: CipherKey, iv: BinaryLike): CipherGCM = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any])).asInstanceOf[CipherGCM]
  inline def createCipheriv(algorithm: CipherGCMTypes, key: CipherKey, iv: BinaryLike, options: CipherGCMOptions): CipherGCM = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[CipherGCM]
  inline def createCipheriv(algorithm: CipherOCBTypes, key: CipherKey, iv: BinaryLike, options: CipherOCBOptions): CipherOCB = (^.asInstanceOf[js.Dynamic].applyDynamic("createCipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[CipherOCB]
  
  inline def createDecipheriv(algorithm: String, key: CipherKey): typings.node.cryptoMod.Decipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Decipher]
  inline def createDecipheriv(algorithm: String, key: CipherKey, iv: Null, options: TransformOptions): typings.node.cryptoMod.Decipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Decipher]
  inline def createDecipheriv(algorithm: String, key: CipherKey, iv: BinaryLike): typings.node.cryptoMod.Decipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Decipher]
  inline def createDecipheriv(algorithm: String, key: CipherKey, iv: BinaryLike, options: TransformOptions): typings.node.cryptoMod.Decipher = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Decipher]
  /**
    * Creates and returns a `Decipher` object that uses the given `algorithm`, `key` and initialization vector (`iv`).
    *
    * The `options` argument controls stream behavior and is optional except when a
    * cipher in CCM or OCB mode (e.g. `'aes-128-ccm'`) is used. In that case, the `authTagLength` option is required and specifies the length of the
    * authentication tag in bytes, see `CCM mode`. In GCM mode, the `authTagLength` option is not required but can be used to restrict accepted authentication tags
    * to those with the specified length.
    * For `chacha20-poly1305`, the `authTagLength` option defaults to 16 bytes.
    *
    * The `algorithm` is dependent on OpenSSL, examples are `'aes192'`, etc. On
    * recent OpenSSL releases, `openssl list -cipher-algorithms` will
    * display the available cipher algorithms.
    *
    * The `key` is the raw key used by the `algorithm` and `iv` is an [initialization vector](https://en.wikipedia.org/wiki/Initialization_vector). Both arguments must be `'utf8'` encoded
    * strings,`Buffers`, `TypedArray`, or `DataView`s. The `key` may optionally be
    * a `KeyObject` of type `secret`. If the cipher does not need
    * an initialization vector, `iv` may be `null`.
    *
    * When passing strings for `key` or `iv`, please consider `caveats when using strings as inputs to cryptographic APIs`.
    *
    * Initialization vectors should be unpredictable and unique; ideally, they will be
    * cryptographically random. They do not have to be secret: IVs are typically just
    * added to ciphertext messages unencrypted. It may sound contradictory that
    * something has to be unpredictable and unique, but does not have to be secret;
    * remember that an attacker must not be able to predict ahead of time what a given
    * IV will be.
    * @since v0.1.94
    * @param options `stream.transform` options
    */
  inline def createDecipheriv(algorithm: CipherCCMTypes, key: CipherKey, iv: BinaryLike, options: CipherCCMOptions): DecipherCCM = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[DecipherCCM]
  inline def createDecipheriv(algorithm: CipherGCMTypes, key: CipherKey, iv: BinaryLike): DecipherGCM = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any])).asInstanceOf[DecipherGCM]
  inline def createDecipheriv(algorithm: CipherGCMTypes, key: CipherKey, iv: BinaryLike, options: CipherGCMOptions): DecipherGCM = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[DecipherGCM]
  inline def createDecipheriv(algorithm: CipherOCBTypes, key: CipherKey, iv: BinaryLike, options: CipherOCBOptions): DecipherOCB = (^.asInstanceOf[js.Dynamic].applyDynamic("createDecipheriv")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], iv.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[DecipherOCB]
  
  /**
    * Creates a `DiffieHellman` key exchange object using the supplied `prime` and an
    * optional specific `generator`.
    *
    * The `generator` argument can be a number, string, or `Buffer`. If `generator` is not specified, the value `2` is used.
    *
    * If `primeEncoding` is specified, `prime` is expected to be a string; otherwise
    * a `Buffer`, `TypedArray`, or `DataView` is expected.
    *
    * If `generatorEncoding` is specified, `generator` is expected to be a string;
    * otherwise a number, `Buffer`, `TypedArray`, or `DataView` is expected.
    * @since v0.11.12
    * @param primeEncoding The `encoding` of the `prime` string.
    * @param [generator=2]
    * @param generatorEncoding The `encoding` of the `generator` string.
    */
  inline def createDiffieHellman(primeLength: Double): typings.node.cryptoMod.DiffieHellman_ = ^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(primeLength.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(primeLength: Double, generator: Double): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(primeLength.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: String, primeEncoding: BinaryToTextEncoding): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], primeEncoding.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(
    prime: String,
    primeEncoding: BinaryToTextEncoding,
    generator: String,
    generatorEncoding: BinaryToTextEncoding
  ): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], primeEncoding.asInstanceOf[js.Any], generator.asInstanceOf[js.Any], generatorEncoding.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: String, primeEncoding: BinaryToTextEncoding, generator: js.typedarray.ArrayBuffer): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], primeEncoding.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: String, primeEncoding: BinaryToTextEncoding, generator: Double): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], primeEncoding.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: String, primeEncoding: BinaryToTextEncoding, generator: ArrayBufferView): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], primeEncoding.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: js.typedarray.ArrayBuffer): typings.node.cryptoMod.DiffieHellman_ = ^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: js.typedarray.ArrayBuffer, generator: String, generatorEncoding: BinaryToTextEncoding): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any], generatorEncoding.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: js.typedarray.ArrayBuffer, generator: js.typedarray.ArrayBuffer): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: js.typedarray.ArrayBuffer, generator: Double): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: js.typedarray.ArrayBuffer, generator: ArrayBufferView): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: ArrayBufferView): typings.node.cryptoMod.DiffieHellman_ = ^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: ArrayBufferView, generator: String, generatorEncoding: BinaryToTextEncoding): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any], generatorEncoding.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: ArrayBufferView, generator: js.typedarray.ArrayBuffer): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: ArrayBufferView, generator: Double): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  inline def createDiffieHellman(prime: ArrayBufferView, generator: ArrayBufferView): typings.node.cryptoMod.DiffieHellman_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellman")(prime.asInstanceOf[js.Any], generator.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.DiffieHellman_]
  
  /**
    * An alias for {@link getDiffieHellman}
    * @since v0.9.3
    */
  inline def createDiffieHellmanGroup(name: String): typings.node.cryptoMod.DiffieHellmanGroup = ^.asInstanceOf[js.Dynamic].applyDynamic("createDiffieHellmanGroup")(name.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.DiffieHellmanGroup]
  
  /**
    * Creates an Elliptic Curve Diffie-Hellman (`ECDH`) key exchange object using a
    * predefined curve specified by the `curveName` string. Use {@link getCurves} to obtain a list of available curve names. On recent
    * OpenSSL releases, `openssl ecparam -list_curves` will also display the name
    * and description of each available elliptic curve.
    * @since v0.11.14
    */
  inline def createECDH(curveName: String): typings.node.cryptoMod.ECDH = ^.asInstanceOf[js.Dynamic].applyDynamic("createECDH")(curveName.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.ECDH]
  
  /**
    * Creates and returns a `Hash` object that can be used to generate hash digests
    * using the given `algorithm`. Optional `options` argument controls stream
    * behavior. For XOF hash functions such as `'shake256'`, the `outputLength` option
    * can be used to specify the desired output length in bytes.
    *
    * The `algorithm` is dependent on the available algorithms supported by the
    * version of OpenSSL on the platform. Examples are `'sha256'`, `'sha512'`, etc.
    * On recent releases of OpenSSL, `openssl list -digest-algorithms` will
    * display the available digest algorithms.
    *
    * Example: generating the sha256 sum of a file
    *
    * ```js
    * import {
    *   createReadStream,
    * } from 'node:fs';
    * import { argv } from 'node:process';
    * const {
    *   createHash,
    * } = await import('node:crypto');
    *
    * const filename = argv[2];
    *
    * const hash = createHash('sha256');
    *
    * const input = createReadStream(filename);
    * input.on('readable', () => {
    *   // Only one element is going to be produced by the
    *   // hash stream.
    *   const data = input.read();
    *   if (data)
    *     hash.update(data);
    *   else {
    *     console.log(`${hash.digest('hex')} ${filename}`);
    *   }
    * });
    * ```
    * @since v0.1.92
    * @param options `stream.transform` options
    */
  inline def createHash(algorithm: String): typings.node.cryptoMod.Hash_ = ^.asInstanceOf[js.Dynamic].applyDynamic("createHash")(algorithm.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.Hash_]
  inline def createHash(algorithm: String, options: HashOptions): typings.node.cryptoMod.Hash_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createHash")(algorithm.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Hash_]
  
  /**
    * Creates and returns an `Hmac` object that uses the given `algorithm` and `key`.
    * Optional `options` argument controls stream behavior.
    *
    * The `algorithm` is dependent on the available algorithms supported by the
    * version of OpenSSL on the platform. Examples are `'sha256'`, `'sha512'`, etc.
    * On recent releases of OpenSSL, `openssl list -digest-algorithms` will
    * display the available digest algorithms.
    *
    * The `key` is the HMAC key used to generate the cryptographic HMAC hash. If it is
    * a `KeyObject`, its type must be `secret`. If it is a string, please consider `caveats when using strings as inputs to cryptographic APIs`. If it was
    * obtained from a cryptographically secure source of entropy, such as {@link randomBytes} or {@link generateKey}, its length should not
    * exceed the block size of `algorithm` (e.g., 512 bits for SHA-256).
    *
    * Example: generating the sha256 HMAC of a file
    *
    * ```js
    * import {
    *   createReadStream,
    * } from 'node:fs';
    * import { argv } from 'node:process';
    * const {
    *   createHmac,
    * } = await import('node:crypto');
    *
    * const filename = argv[2];
    *
    * const hmac = createHmac('sha256', 'a secret');
    *
    * const input = createReadStream(filename);
    * input.on('readable', () => {
    *   // Only one element is going to be produced by the
    *   // hash stream.
    *   const data = input.read();
    *   if (data)
    *     hmac.update(data);
    *   else {
    *     console.log(`${hmac.digest('hex')} ${filename}`);
    *   }
    * });
    * ```
    * @since v0.1.94
    * @param options `stream.transform` options
    */
  inline def createHmac(algorithm: String, key: BinaryLike): typings.node.cryptoMod.Hmac = (^.asInstanceOf[js.Dynamic].applyDynamic("createHmac")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Hmac]
  inline def createHmac(algorithm: String, key: BinaryLike, options: TransformOptions): typings.node.cryptoMod.Hmac = (^.asInstanceOf[js.Dynamic].applyDynamic("createHmac")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Hmac]
  inline def createHmac(algorithm: String, key: typings.node.cryptoMod.KeyObject): typings.node.cryptoMod.Hmac = (^.asInstanceOf[js.Dynamic].applyDynamic("createHmac")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Hmac]
  inline def createHmac(algorithm: String, key: typings.node.cryptoMod.KeyObject, options: TransformOptions): typings.node.cryptoMod.Hmac = (^.asInstanceOf[js.Dynamic].applyDynamic("createHmac")(algorithm.asInstanceOf[js.Any], key.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Hmac]
  
  inline def createPrivateKey(key: String): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPrivateKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  inline def createPrivateKey(key: Buffer): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPrivateKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  inline def createPrivateKey(key: JsonWebKeyInput): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPrivateKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  /**
    * Creates and returns a new key object containing a private key. If `key` is a
    * string or `Buffer`, `format` is assumed to be `'pem'`; otherwise, `key` must be an object with the properties described above.
    *
    * If the private key is encrypted, a `passphrase` must be specified. The length
    * of the passphrase is limited to 1024 bytes.
    * @since v11.6.0
    */
  inline def createPrivateKey(key: PrivateKeyInput): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPrivateKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  
  inline def createPublicKey(key: String): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPublicKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  inline def createPublicKey(key: Buffer): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPublicKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  inline def createPublicKey(key: JsonWebKeyInput): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPublicKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  inline def createPublicKey(key: typings.node.cryptoMod.KeyObject): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPublicKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  /**
    * Creates and returns a new key object containing a public key. If `key` is a
    * string or `Buffer`, `format` is assumed to be `'pem'`; if `key` is a `KeyObject` with type `'private'`, the public key is derived from the given private key;
    * otherwise, `key` must be an object with the properties described above.
    *
    * If the format is `'pem'`, the `'key'` may also be an X.509 certificate.
    *
    * Because public keys can be derived from private keys, a private key may be
    * passed instead of a public key. In that case, this function behaves as if {@link createPrivateKey} had been called, except that the type of the
    * returned `KeyObject` will be `'public'` and that the private key cannot be
    * extracted from the returned `KeyObject`. Similarly, if a `KeyObject` with type `'private'` is given, a new `KeyObject` with type `'public'` will be returned
    * and it will be impossible to extract the private key from the returned object.
    * @since v11.6.0
    */
  inline def createPublicKey(key: PublicKeyInput): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createPublicKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  
  inline def createSecretKey(key: String, encoding: BufferEncoding): typings.node.cryptoMod.KeyObject = (^.asInstanceOf[js.Dynamic].applyDynamic("createSecretKey")(key.asInstanceOf[js.Any], encoding.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.KeyObject]
  /**
    * Creates and returns a new key object containing a secret key for symmetric
    * encryption or `Hmac`.
    * @since v11.6.0
    * @param encoding The string encoding when `key` is a string.
    */
  inline def createSecretKey(key: ArrayBufferView): typings.node.cryptoMod.KeyObject = ^.asInstanceOf[js.Dynamic].applyDynamic("createSecretKey")(key.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.KeyObject]
  
  /**
    * Creates and returns a `Sign` object that uses the given `algorithm`. Use {@link getHashes} to obtain the names of the available digest algorithms.
    * Optional `options` argument controls the `stream.Writable` behavior.
    *
    * In some cases, a `Sign` instance can be created using the name of a signature
    * algorithm, such as `'RSA-SHA256'`, instead of a digest algorithm. This will use
    * the corresponding digest algorithm. This does not work for all signature
    * algorithms, such as `'ecdsa-with-SHA256'`, so it is best to always use digest
    * algorithm names.
    * @since v0.1.92
    * @param options `stream.Writable` options
    */
  inline def createSign(algorithm: String): typings.node.cryptoMod.Sign_ = ^.asInstanceOf[js.Dynamic].applyDynamic("createSign")(algorithm.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.Sign_]
  inline def createSign(algorithm: String, options: WritableOptions): typings.node.cryptoMod.Sign_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createSign")(algorithm.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Sign_]
  
  /**
    * Creates and returns a `Verify` object that uses the given algorithm.
    * Use {@link getHashes} to obtain an array of names of the available
    * signing algorithms. Optional `options` argument controls the `stream.Writable` behavior.
    *
    * In some cases, a `Verify` instance can be created using the name of a signature
    * algorithm, such as `'RSA-SHA256'`, instead of a digest algorithm. This will use
    * the corresponding digest algorithm. This does not work for all signature
    * algorithms, such as `'ecdsa-with-SHA256'`, so it is best to always use digest
    * algorithm names.
    * @since v0.1.92
    * @param options `stream.Writable` options
    */
  inline def createVerify(algorithm: String): typings.node.cryptoMod.Verify_ = ^.asInstanceOf[js.Dynamic].applyDynamic("createVerify")(algorithm.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.Verify_]
  inline def createVerify(algorithm: String, options: WritableOptions): typings.node.cryptoMod.Verify_ = (^.asInstanceOf[js.Dynamic].applyDynamic("createVerify")(algorithm.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.Verify_]
  
  /**
    * Computes the Diffie-Hellman secret based on a `privateKey` and a `publicKey`.
    * Both keys must have the same `asymmetricKeyType`, which must be one of `'dh'` (for Diffie-Hellman), `'ec'` (for ECDH), `'x448'`, or `'x25519'` (for ECDH-ES).
    * @since v13.9.0, v12.17.0
    */
  inline def diffieHellman(options: PrivateKey): Buffer = ^.asInstanceOf[js.Dynamic].applyDynamic("diffieHellman")(options.asInstanceOf[js.Any]).asInstanceOf[Buffer]
  
  /** @deprecated since v10.0.0 */
  @JSImport("node:crypto", "fips")
  @js.native
  val fips: Boolean = js.native
  
  /**
    * Asynchronously generates a new random secret key of the given `length`. The `type` will determine which validations will be performed on the `length`.
    *
    * ```js
    * const {
    *   generateKey,
    * } = await import('node:crypto');
    *
    * generateKey('hmac', { length: 512 }, (err, key) => {
    *   if (err) throw err;
    *   console.log(key.export().toString('hex'));  // 46e..........620
    * });
    * ```
    *
    * The size of a generated HMAC key should not exceed the block size of the
    * underlying hash function. See {@link createHmac} for more information.
    * @since v15.0.0
    * @param type The intended use of the generated secret key. Currently accepted values are `'hmac'` and `'aes'`.
    */
  inline def generateKey(
    `type`: hmac | aes,
    options: Length,
    callback: js.Function2[/* err */ js.Error | Null, /* key */ typings.node.cryptoMod.KeyObject, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKey")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  inline def generateKeyPair(
    `type`: `rsa-pss`,
    options: RSAPSSKeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: `rsa-pss`,
    options: RSAPSSKeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: dsa,
    options: DSAKeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: dsa,
    options: DSAKeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ec,
    options: ECKeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ec,
    options: ECKeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ed25519,
    options: Unit,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ed25519,
    options: ED25519KeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ed25519,
    options: ED25519KeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ed448,
    options: Unit,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ed448,
    options: ED448KeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: ed448,
    options: ED448KeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: rsa,
    options: RSAKeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  /**
    * Generates a new asymmetric key pair of the given `type`. RSA, RSA-PSS, DSA, EC,
    * Ed25519, Ed448, X25519, X448, and DH are currently supported.
    *
    * If a `publicKeyEncoding` or `privateKeyEncoding` was specified, this function
    * behaves as if `keyObject.export()` had been called on its result. Otherwise,
    * the respective part of the key is returned as a `KeyObject`.
    *
    * It is recommended to encode public keys as `'spki'` and private keys as `'pkcs8'` with encryption for long-term storage:
    *
    * ```js
    * const {
    *   generateKeyPair,
    * } = await import('node:crypto');
    *
    * generateKeyPair('rsa', {
    *   modulusLength: 4096,
    *   publicKeyEncoding: {
    *     type: 'spki',
    *     format: 'pem',
    *   },
    *   privateKeyEncoding: {
    *     type: 'pkcs8',
    *     format: 'pem',
    *     cipher: 'aes-256-cbc',
    *     passphrase: 'top secret',
    *   },
    * }, (err, publicKey, privateKey) => {
    *   // Handle errors and use the generated key pair.
    * });
    * ```
    *
    * On completion, `callback` will be called with `err` set to `undefined` and `publicKey` / `privateKey` representing the generated key pair.
    *
    * If this method is invoked as its `util.promisify()` ed version, it returns
    * a `Promise` for an `Object` with `publicKey` and `privateKey` properties.
    * @since v10.12.0
    * @param type Must be `'rsa'`, `'rsa-pss'`, `'dsa'`, `'ec'`, `'ed25519'`, `'ed448'`, `'x25519'`, `'x448'`, or `'dh'`.
    */
  inline def generateKeyPair(
    `type`: rsa,
    options: RSAKeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: x25519,
    options: Unit,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: x25519,
    options: X25519KeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: x25519,
    options: X25519KeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: x448,
    options: Unit,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: x448,
    options: X448KeyPairKeyObjectOptions,
    callback: js.Function3[
      /* err */ js.Error | Null, 
      /* publicKey */ typings.node.cryptoMod.KeyObject, 
      /* privateKey */ typings.node.cryptoMod.KeyObject, 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generateKeyPair(
    `type`: x448,
    options: X448KeyPairOptions[der | pem, der | pem],
    callback: js.Function3[
      js.Error | Null, 
      (/* publicKey */ Buffer) | (/* publicKey */ String), 
      (/* privateKey */ Buffer) | (/* privateKey */ String), 
      Unit
    ]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPair")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  inline def generateKeyPairSync(`type`: `rsa-pss`, options: RSAPSSKeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  inline def generateKeyPairSync(`type`: dsa, options: DSAKeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  inline def generateKeyPairSync(`type`: ec, options: ECKeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  inline def generateKeyPairSync(`type`: ed25519, options: ED25519KeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  inline def generateKeyPairSync(`type`: ed448, options: ED448KeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  /**
    * Generates a new asymmetric key pair of the given `type`. RSA, RSA-PSS, DSA, EC,
    * Ed25519, Ed448, X25519, X448, and DH are currently supported.
    *
    * If a `publicKeyEncoding` or `privateKeyEncoding` was specified, this function
    * behaves as if `keyObject.export()` had been called on its result. Otherwise,
    * the respective part of the key is returned as a `KeyObject`.
    *
    * When encoding public keys, it is recommended to use `'spki'`. When encoding
    * private keys, it is recommended to use `'pkcs8'` with a strong passphrase,
    * and to keep the passphrase confidential.
    *
    * ```js
    * const {
    *   generateKeyPairSync,
    * } = await import('node:crypto');
    *
    * const {
    *   publicKey,
    *   privateKey,
    * } = generateKeyPairSync('rsa', {
    *   modulusLength: 4096,
    *   publicKeyEncoding: {
    *     type: 'spki',
    *     format: 'pem',
    *   },
    *   privateKeyEncoding: {
    *     type: 'pkcs8',
    *     format: 'pem',
    *     cipher: 'aes-256-cbc',
    *     passphrase: 'top secret',
    *   },
    * });
    * ```
    *
    * The return value `{ publicKey, privateKey }` represents the generated key pair.
    * When PEM encoding was selected, the respective key will be a string, otherwise
    * it will be a buffer containing the data encoded as DER.
    * @since v10.12.0
    * @param type Must be `'rsa'`, `'rsa-pss'`, `'dsa'`, `'ec'`, `'ed25519'`, `'ed448'`, `'x25519'`, `'x448'`, or `'dh'`.
    */
  inline def generateKeyPairSync(`type`: rsa, options: RSAKeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  inline def generateKeyPairSync(`type`: x25519, options: X25519KeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  inline def generateKeyPairSync(`type`: x448, options: X448KeyPairOptions[der | pem, der | pem]): KeyPairSyncResult[String, String] = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairSyncResult[String, String]]
  
  inline def generateKeyPairSync_dsa(`type`: dsa, options: DSAKeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_ec(`type`: ec, options: ECKeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_ed25519(`type`: ed25519): KeyPairKeyObjectResult = ^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any]).asInstanceOf[KeyPairKeyObjectResult]
  inline def generateKeyPairSync_ed25519(`type`: ed25519, options: ED25519KeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_ed448(`type`: ed448): KeyPairKeyObjectResult = ^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any]).asInstanceOf[KeyPairKeyObjectResult]
  inline def generateKeyPairSync_ed448(`type`: ed448, options: ED448KeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_rsa(`type`: rsa, options: RSAKeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_rsapss(`type`: `rsa-pss`, options: RSAPSSKeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_x25519(`type`: x25519): KeyPairKeyObjectResult = ^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any]).asInstanceOf[KeyPairKeyObjectResult]
  inline def generateKeyPairSync_x25519(`type`: x25519, options: X25519KeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  inline def generateKeyPairSync_x448(`type`: x448): KeyPairKeyObjectResult = ^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any]).asInstanceOf[KeyPairKeyObjectResult]
  inline def generateKeyPairSync_x448(`type`: x448, options: X448KeyPairKeyObjectOptions): KeyPairKeyObjectResult = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeyPairSync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[KeyPairKeyObjectResult]
  
  /**
    * Synchronously generates a new random secret key of the given `length`. The `type` will determine which validations will be performed on the `length`.
    *
    * ```js
    * const {
    *   generateKeySync,
    * } = await import('node:crypto');
    *
    * const key = generateKeySync('hmac', { length: 512 });
    * console.log(key.export().toString('hex'));  // e89..........41e
    * ```
    *
    * The size of a generated HMAC key should not exceed the block size of the
    * underlying hash function. See {@link createHmac} for more information.
    * @since v15.0.0
    * @param type The intended use of the generated secret key. Currently accepted values are `'hmac'` and `'aes'`.
    */
  inline def generateKeySync(`type`: hmac | aes, options: Length): typings.node.cryptoMod.KeyObject = (^.asInstanceOf[js.Dynamic].applyDynamic("generateKeySync")(`type`.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[typings.node.cryptoMod.KeyObject]
  
  /**
    * Generates a pseudorandom prime of `size` bits.
    *
    * If `options.safe` is `true`, the prime will be a safe prime -- that is, `(prime - 1) / 2` will also be a prime.
    *
    * The `options.add` and `options.rem` parameters can be used to enforce additional
    * requirements, e.g., for Diffie-Hellman:
    *
    * * If `options.add` and `options.rem` are both set, the prime will satisfy the
    * condition that `prime % add = rem`.
    * * If only `options.add` is set and `options.safe` is not `true`, the prime will
    * satisfy the condition that `prime % add = 1`.
    * * If only `options.add` is set and `options.safe` is set to `true`, the prime
    * will instead satisfy the condition that `prime % add = 3`. This is necessary
    * because `prime % add = 1` for `options.add > 2` would contradict the condition
    * enforced by `options.safe`.
    * * `options.rem` is ignored if `options.add` is not given.
    *
    * Both `options.add` and `options.rem` must be encoded as big-endian sequences
    * if given as an `ArrayBuffer`, `SharedArrayBuffer`, `TypedArray`, `Buffer`, or `DataView`.
    *
    * By default, the prime is encoded as a big-endian sequence of octets
    * in an [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer). If the `bigint` option is `true`, then a
    * [bigint](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) is provided.
    * @since v15.8.0
    * @param size The size (in bits) of the prime to generate.
    */
  inline def generatePrime(
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* prime */ js.typedarray.ArrayBuffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrime")(size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generatePrime(
    size: Double,
    options: GeneratePrimeOptionsArrayBuffer,
    callback: js.Function2[/* err */ js.Error | Null, /* prime */ js.typedarray.ArrayBuffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrime")(size.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generatePrime(
    size: Double,
    options: GeneratePrimeOptionsBigInt,
    callback: js.Function2[/* err */ js.Error | Null, /* prime */ js.BigInt, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrime")(size.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def generatePrime(
    size: Double,
    options: GeneratePrimeOptions,
    callback: js.Function2[/* err */ js.Error | Null, /* prime */ js.typedarray.ArrayBuffer | js.BigInt, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrime")(size.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Generates a pseudorandom prime of `size` bits.
    *
    * If `options.safe` is `true`, the prime will be a safe prime -- that is, `(prime - 1) / 2` will also be a prime.
    *
    * The `options.add` and `options.rem` parameters can be used to enforce additional
    * requirements, e.g., for Diffie-Hellman:
    *
    * * If `options.add` and `options.rem` are both set, the prime will satisfy the
    * condition that `prime % add = rem`.
    * * If only `options.add` is set and `options.safe` is not `true`, the prime will
    * satisfy the condition that `prime % add = 1`.
    * * If only `options.add` is set and `options.safe` is set to `true`, the prime
    * will instead satisfy the condition that `prime % add = 3`. This is necessary
    * because `prime % add = 1` for `options.add > 2` would contradict the condition
    * enforced by `options.safe`.
    * * `options.rem` is ignored if `options.add` is not given.
    *
    * Both `options.add` and `options.rem` must be encoded as big-endian sequences
    * if given as an `ArrayBuffer`, `SharedArrayBuffer`, `TypedArray`, `Buffer`, or `DataView`.
    *
    * By default, the prime is encoded as a big-endian sequence of octets
    * in an [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer). If the `bigint` option is `true`, then a
    * [bigint](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) is provided.
    * @since v15.8.0
    * @param size The size (in bits) of the prime to generate.
    */
  inline def generatePrimeSync(size: Double): js.typedarray.ArrayBuffer = ^.asInstanceOf[js.Dynamic].applyDynamic("generatePrimeSync")(size.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.ArrayBuffer]
  inline def generatePrimeSync(size: Double, options: GeneratePrimeOptions): js.typedarray.ArrayBuffer | js.BigInt = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrimeSync")(size.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.ArrayBuffer | js.BigInt]
  inline def generatePrimeSync(size: Double, options: GeneratePrimeOptionsArrayBuffer): js.typedarray.ArrayBuffer = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrimeSync")(size.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.ArrayBuffer]
  inline def generatePrimeSync(size: Double, options: GeneratePrimeOptionsBigInt): js.BigInt = (^.asInstanceOf[js.Dynamic].applyDynamic("generatePrimeSync")(size.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[js.BigInt]
  
  /**
    * Returns information about a given cipher.
    *
    * Some ciphers accept variable length keys and initialization vectors. By default,
    * the `crypto.getCipherInfo()` method will return the default values for these
    * ciphers. To test if a given key length or iv length is acceptable for given
    * cipher, use the `keyLength` and `ivLength` options. If the given values are
    * unacceptable, `undefined` will be returned.
    * @since v15.0.0
    * @param nameOrNid The name or nid of the cipher to query.
    */
  inline def getCipherInfo(nameOrNid: String): js.UndefOr[CipherInfo] = ^.asInstanceOf[js.Dynamic].applyDynamic("getCipherInfo")(nameOrNid.asInstanceOf[js.Any]).asInstanceOf[js.UndefOr[CipherInfo]]
  inline def getCipherInfo(nameOrNid: String, options: CipherInfoOptions): js.UndefOr[CipherInfo] = (^.asInstanceOf[js.Dynamic].applyDynamic("getCipherInfo")(nameOrNid.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[js.UndefOr[CipherInfo]]
  inline def getCipherInfo(nameOrNid: Double): js.UndefOr[CipherInfo] = ^.asInstanceOf[js.Dynamic].applyDynamic("getCipherInfo")(nameOrNid.asInstanceOf[js.Any]).asInstanceOf[js.UndefOr[CipherInfo]]
  inline def getCipherInfo(nameOrNid: Double, options: CipherInfoOptions): js.UndefOr[CipherInfo] = (^.asInstanceOf[js.Dynamic].applyDynamic("getCipherInfo")(nameOrNid.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[js.UndefOr[CipherInfo]]
  
  /**
    * ```js
    * const {
    *   getCiphers,
    * } = await import('node:crypto');
    *
    * console.log(getCiphers()); // ['aes-128-cbc', 'aes-128-ccm', ...]
    * ```
    * @since v0.9.3
    * @return An array with the names of the supported cipher algorithms.
    */
  inline def getCiphers(): js.Array[String] = ^.asInstanceOf[js.Dynamic].applyDynamic("getCiphers")().asInstanceOf[js.Array[String]]
  
  /**
    * ```js
    * const {
    *   getCurves,
    * } = await import('node:crypto');
    *
    * console.log(getCurves()); // ['Oakley-EC2N-3', 'Oakley-EC2N-4', ...]
    * ```
    * @since v2.3.0
    * @return An array with the names of the supported elliptic curves.
    */
  inline def getCurves(): js.Array[String] = ^.asInstanceOf[js.Dynamic].applyDynamic("getCurves")().asInstanceOf[js.Array[String]]
  
  /**
    * Creates a predefined `DiffieHellmanGroup` key exchange object. The
    * supported groups are listed in the documentation for `DiffieHellmanGroup`.
    *
    * The returned object mimics the interface of objects created by {@link createDiffieHellman}, but will not allow changing
    * the keys (with `diffieHellman.setPublicKey()`, for example). The
    * advantage of using this method is that the parties do not have to
    * generate nor exchange a group modulus beforehand, saving both processor
    * and communication time.
    *
    * Example (obtaining a shared secret):
    *
    * ```js
    * const {
    *   getDiffieHellman,
    * } = await import('node:crypto');
    * const alice = getDiffieHellman('modp14');
    * const bob = getDiffieHellman('modp14');
    *
    * alice.generateKeys();
    * bob.generateKeys();
    *
    * const aliceSecret = alice.computeSecret(bob.getPublicKey(), null, 'hex');
    * const bobSecret = bob.computeSecret(alice.getPublicKey(), null, 'hex');
    *
    * // aliceSecret and bobSecret should be the same
    * console.log(aliceSecret === bobSecret);
    * ```
    * @since v0.7.5
    */
  inline def getDiffieHellman(groupName: String): typings.node.cryptoMod.DiffieHellmanGroup = ^.asInstanceOf[js.Dynamic].applyDynamic("getDiffieHellman")(groupName.asInstanceOf[js.Any]).asInstanceOf[typings.node.cryptoMod.DiffieHellmanGroup]
  
  /**
    * @since v10.0.0
    * @return `1` if and only if a FIPS compliant crypto provider is currently in use, `0` otherwise. A future semver-major release may change the return type of this API to a {boolean}.
    */
  inline def getFips(): `1` | `0` = ^.asInstanceOf[js.Dynamic].applyDynamic("getFips")().asInstanceOf[`1` | `0`]
  
  /**
    * ```js
    * const {
    *   getHashes,
    * } = await import('node:crypto');
    *
    * console.log(getHashes()); // ['DSA', 'DSA-SHA', 'DSA-SHA1', ...]
    * ```
    * @since v0.9.3
    * @return An array of the names of the supported hash algorithms, such as `'RSA-SHA256'`. Hash algorithms are also called "digest" algorithms.
    */
  inline def getHashes(): js.Array[String] = ^.asInstanceOf[js.Dynamic].applyDynamic("getHashes")().asInstanceOf[js.Array[String]]
  
  inline def getRandomValues(typedArray: js.typedarray.ArrayBuffer): js.typedarray.ArrayBuffer = ^.asInstanceOf[js.Dynamic].applyDynamic("getRandomValues")(typedArray.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.ArrayBuffer]
  /**
    * A convenient alias for {@link webcrypto.getRandomValues}. This
    * implementation is not compliant with the Web Crypto spec, to write
    * web-compatible code use {@link webcrypto.getRandomValues} instead.
    * @since v17.4.0
    * @return Returns `typedArray`.
    */
  inline def getRandomValues(typedArray: js.typedarray.ArrayBufferView): js.typedarray.ArrayBufferView = ^.asInstanceOf[js.Dynamic].applyDynamic("getRandomValues")(typedArray.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.ArrayBufferView]
  
  /**
    * A utility for creating one-shot hash digests of data. It can be faster than the object-based `crypto.createHash()` when hashing a smaller amount of data
    * (<= 5MB) that's readily available. If the data can be big or if it is streamed, it's still recommended to use `crypto.createHash()` instead. The `algorithm`
    * is dependent on the available algorithms supported by the version of OpenSSL on the platform. Examples are `'sha256'`, `'sha512'`, etc. On recent releases
    * of OpenSSL, `openssl list -digest-algorithms` will display the available digest algorithms.
    *
    * Example:
    *
    * ```js
    * const crypto = require('node:crypto');
    * const { Buffer } = require('node:buffer');
    *
    * // Hashing a string and return the result as a hex-encoded string.
    * const string = 'Node.js';
    * // 10b3493287f831e81a438811a1ffba01f8cec4b7
    * console.log(crypto.hash('sha1', string));
    *
    * // Encode a base64-encoded string into a Buffer, hash it and return
    * // the result as a buffer.
    * const base64 = 'Tm9kZS5qcw==';
    * // <Buffer 10 b3 49 32 87 f8 31 e8 1a 43 88 11 a1 ff ba 01 f8 ce c4 b7>
    * console.log(crypto.hash('sha1', Buffer.from(base64, 'base64'), 'buffer'));
    * ```
    * @since v21.7.0, v20.12.0
    * @param data When `data` is a string, it will be encoded as UTF-8 before being hashed. If a different input encoding is desired for a string input, user
    *             could encode the string into a `TypedArray` using either `TextEncoder` or `Buffer.from()` and passing the encoded `TypedArray` into this API instead.
    * @param [outputEncoding='hex'] [Encoding](https://nodejs.org/docs/latest-v22.x/api/buffer.html#buffers-and-character-encodings) used to encode the returned digest.
    */
  inline def hash(algorithm: String, data: BinaryLike): String = (^.asInstanceOf[js.Dynamic].applyDynamic("hash")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any])).asInstanceOf[String]
  inline def hash(algorithm: String, data: BinaryLike, outputEncoding: BinaryToTextEncoding): String = (^.asInstanceOf[js.Dynamic].applyDynamic("hash")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any])).asInstanceOf[String]
  
  inline def hash_Union(algorithm: String, data: BinaryLike): String | Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("hash")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any])).asInstanceOf[String | Buffer]
  inline def hash_Union(algorithm: String, data: BinaryLike, outputEncoding: BinaryToTextEncoding): String | Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("hash")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any])).asInstanceOf[String | Buffer]
  
  inline def hash_buffer(algorithm: String, data: BinaryLike, outputEncoding: buffer_): String | Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("hash")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any])).asInstanceOf[String | Buffer]
  
  inline def hash_buffer_Buffer(algorithm: String, data: BinaryLike, outputEncoding: buffer_): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("hash")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], outputEncoding.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  /**
    * HKDF is a simple key derivation function defined in RFC 5869\. The given `ikm`, `salt` and `info` are used with the `digest` to derive a key of `keylen` bytes.
    *
    * The supplied `callback` function is called with two arguments: `err` and `derivedKey`. If an errors occurs while deriving the key, `err` will be set;
    * otherwise `err` will be `null`. The successfully generated `derivedKey` will
    * be passed to the callback as an [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer). An error will be thrown if any
    * of the input arguments specify invalid values or types.
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const {
    *   hkdf,
    * } = await import('node:crypto');
    *
    * hkdf('sha512', 'key', 'salt', 'info', 64, (err, derivedKey) => {
    *   if (err) throw err;
    *   console.log(Buffer.from(derivedKey).toString('hex'));  // '24156e2...5391653'
    * });
    * ```
    * @since v15.0.0
    * @param digest The digest algorithm to use.
    * @param ikm The input keying material. Must be provided but can be zero-length.
    * @param salt The salt value. Must be provided but can be zero-length.
    * @param info Additional info value. Must be provided but can be zero-length, and cannot be more than 1024 bytes.
    * @param keylen The length of the key to generate. Must be greater than 0. The maximum allowable value is `255` times the number of bytes produced by the selected digest function (e.g. `sha512`
    * generates 64-byte hashes, making the maximum HKDF output 16320 bytes).
    */
  inline def hkdf(
    digest: String,
    irm: BinaryLike,
    salt: BinaryLike,
    info: BinaryLike,
    keylen: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* derivedKey */ js.typedarray.ArrayBuffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("hkdf")(digest.asInstanceOf[js.Any], irm.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], info.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def hkdf(
    digest: String,
    irm: typings.node.cryptoMod.KeyObject,
    salt: BinaryLike,
    info: BinaryLike,
    keylen: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* derivedKey */ js.typedarray.ArrayBuffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("hkdf")(digest.asInstanceOf[js.Any], irm.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], info.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Provides a synchronous HKDF key derivation function as defined in RFC 5869\. The
    * given `ikm`, `salt` and `info` are used with the `digest` to derive a key of `keylen` bytes.
    *
    * The successfully generated `derivedKey` will be returned as an [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer).
    *
    * An error will be thrown if any of the input arguments specify invalid values or
    * types, or if the derived key cannot be generated.
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const {
    *   hkdfSync,
    * } = await import('node:crypto');
    *
    * const derivedKey = hkdfSync('sha512', 'key', 'salt', 'info', 64);
    * console.log(Buffer.from(derivedKey).toString('hex'));  // '24156e2...5391653'
    * ```
    * @since v15.0.0
    * @param digest The digest algorithm to use.
    * @param ikm The input keying material. Must be provided but can be zero-length.
    * @param salt The salt value. Must be provided but can be zero-length.
    * @param info Additional info value. Must be provided but can be zero-length, and cannot be more than 1024 bytes.
    * @param keylen The length of the key to generate. Must be greater than 0. The maximum allowable value is `255` times the number of bytes produced by the selected digest function (e.g. `sha512`
    * generates 64-byte hashes, making the maximum HKDF output 16320 bytes).
    */
  inline def hkdfSync(digest: String, ikm: BinaryLike, salt: BinaryLike, info: BinaryLike, keylen: Double): js.typedarray.ArrayBuffer = (^.asInstanceOf[js.Dynamic].applyDynamic("hkdfSync")(digest.asInstanceOf[js.Any], ikm.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], info.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.ArrayBuffer]
  inline def hkdfSync(
    digest: String,
    ikm: typings.node.cryptoMod.KeyObject,
    salt: BinaryLike,
    info: BinaryLike,
    keylen: Double
  ): js.typedarray.ArrayBuffer = (^.asInstanceOf[js.Dynamic].applyDynamic("hkdfSync")(digest.asInstanceOf[js.Any], ikm.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], info.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.ArrayBuffer]
  
  /**
    * Provides an asynchronous Password-Based Key Derivation Function 2 (PBKDF2)
    * implementation. A selected HMAC digest algorithm specified by `digest` is
    * applied to derive a key of the requested byte length (`keylen`) from the `password`, `salt` and `iterations`.
    *
    * The supplied `callback` function is called with two arguments: `err` and `derivedKey`. If an error occurs while deriving the key, `err` will be set;
    * otherwise `err` will be `null`. By default, the successfully generated `derivedKey` will be passed to the callback as a `Buffer`. An error will be
    * thrown if any of the input arguments specify invalid values or types.
    *
    * The `iterations` argument must be a number set as high as possible. The
    * higher the number of iterations, the more secure the derived key will be,
    * but will take a longer amount of time to complete.
    *
    * The `salt` should be as unique as possible. It is recommended that a salt is
    * random and at least 16 bytes long. See [NIST SP 800-132](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-132.pdf) for details.
    *
    * When passing strings for `password` or `salt`, please consider `caveats when using strings as inputs to cryptographic APIs`.
    *
    * ```js
    * const {
    *   pbkdf2,
    * } = await import('node:crypto');
    *
    * pbkdf2('secret', 'salt', 100000, 64, 'sha512', (err, derivedKey) => {
    *   if (err) throw err;
    *   console.log(derivedKey.toString('hex'));  // '3745e48...08d59ae'
    * });
    * ```
    *
    * An array of supported digest functions can be retrieved using {@link getHashes}.
    *
    * This API uses libuv's threadpool, which can have surprising and
    * negative performance implications for some applications; see the `UV_THREADPOOL_SIZE` documentation for more information.
    * @since v0.5.5
    */
  inline def pbkdf2(
    password: BinaryLike,
    salt: BinaryLike,
    iterations: Double,
    keylen: Double,
    digest: String,
    callback: js.Function2[/* err */ js.Error | Null, /* derivedKey */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("pbkdf2")(password.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], iterations.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], digest.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Provides a synchronous Password-Based Key Derivation Function 2 (PBKDF2)
    * implementation. A selected HMAC digest algorithm specified by `digest` is
    * applied to derive a key of the requested byte length (`keylen`) from the `password`, `salt` and `iterations`.
    *
    * If an error occurs an `Error` will be thrown, otherwise the derived key will be
    * returned as a `Buffer`.
    *
    * The `iterations` argument must be a number set as high as possible. The
    * higher the number of iterations, the more secure the derived key will be,
    * but will take a longer amount of time to complete.
    *
    * The `salt` should be as unique as possible. It is recommended that a salt is
    * random and at least 16 bytes long. See [NIST SP 800-132](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-132.pdf) for details.
    *
    * When passing strings for `password` or `salt`, please consider `caveats when using strings as inputs to cryptographic APIs`.
    *
    * ```js
    * const {
    *   pbkdf2Sync,
    * } = await import('node:crypto');
    *
    * const key = pbkdf2Sync('secret', 'salt', 100000, 64, 'sha512');
    * console.log(key.toString('hex'));  // '3745e48...08d59ae'
    * ```
    *
    * An array of supported digest functions can be retrieved using {@link getHashes}.
    * @since v0.9.3
    */
  inline def pbkdf2Sync(password: BinaryLike, salt: BinaryLike, iterations: Double, keylen: Double, digest: String): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("pbkdf2Sync")(password.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], iterations.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], digest.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  inline def privateDecrypt(privateKey: KeyLike, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("privateDecrypt")(privateKey.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  /**
    * Decrypts `buffer` with `privateKey`. `buffer` was previously encrypted using
    * the corresponding public key, for example using {@link publicEncrypt}.
    *
    * If `privateKey` is not a `KeyObject`, this function behaves as if `privateKey` had been passed to {@link createPrivateKey}. If it is an
    * object, the `padding` property can be passed. Otherwise, this function uses `RSA_PKCS1_OAEP_PADDING`.
    * @since v0.11.14
    */
  inline def privateDecrypt(privateKey: RsaPrivateKey, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("privateDecrypt")(privateKey.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  inline def privateEncrypt(privateKey: KeyLike, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("privateEncrypt")(privateKey.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  /**
    * Encrypts `buffer` with `privateKey`. The returned data can be decrypted using
    * the corresponding public key, for example using {@link publicDecrypt}.
    *
    * If `privateKey` is not a `KeyObject`, this function behaves as if `privateKey` had been passed to {@link createPrivateKey}. If it is an
    * object, the `padding` property can be passed. Otherwise, this function uses `RSA_PKCS1_PADDING`.
    * @since v1.1.0
    */
  inline def privateEncrypt(privateKey: RsaPrivateKey, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("privateEncrypt")(privateKey.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  inline def pseudoRandomBytes(size: Double): Buffer = ^.asInstanceOf[js.Dynamic].applyDynamic("pseudoRandomBytes")(size.asInstanceOf[js.Any]).asInstanceOf[Buffer]
  inline def pseudoRandomBytes(size: Double, callback: js.Function2[/* err */ js.Error | Null, /* buf */ Buffer, Unit]): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("pseudoRandomBytes")(size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  inline def publicDecrypt(key: KeyLike, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("publicDecrypt")(key.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def publicDecrypt(key: RsaPrivateKey, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("publicDecrypt")(key.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  /**
    * Decrypts `buffer` with `key`.`buffer` was previously encrypted using
    * the corresponding private key, for example using {@link privateEncrypt}.
    *
    * If `key` is not a `KeyObject`, this function behaves as if `key` had been passed to {@link createPublicKey}. If it is an
    * object, the `padding` property can be passed. Otherwise, this function uses `RSA_PKCS1_PADDING`.
    *
    * Because RSA public keys can be derived from private keys, a private key may
    * be passed instead of a public key.
    * @since v1.1.0
    */
  inline def publicDecrypt(key: RsaPublicKey, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("publicDecrypt")(key.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  inline def publicEncrypt(key: KeyLike, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("publicEncrypt")(key.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def publicEncrypt(key: RsaPrivateKey, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("publicEncrypt")(key.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  /**
    * Encrypts the content of `buffer` with `key` and returns a new `Buffer` with encrypted content. The returned data can be decrypted using
    * the corresponding private key, for example using {@link privateDecrypt}.
    *
    * If `key` is not a `KeyObject`, this function behaves as if `key` had been passed to {@link createPublicKey}. If it is an
    * object, the `padding` property can be passed. Otherwise, this function uses `RSA_PKCS1_OAEP_PADDING`.
    *
    * Because RSA public keys can be derived from private keys, a private key may
    * be passed instead of a public key.
    * @since v0.11.14
    */
  inline def publicEncrypt(key: RsaPublicKey, buffer: ArrayBufferView): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("publicEncrypt")(key.asInstanceOf[js.Any], buffer.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  /**
    * Generates cryptographically strong pseudorandom data. The `size` argument
    * is a number indicating the number of bytes to generate.
    *
    * If a `callback` function is provided, the bytes are generated asynchronously
    * and the `callback` function is invoked with two arguments: `err` and `buf`.
    * If an error occurs, `err` will be an `Error` object; otherwise it is `null`. The `buf` argument is a `Buffer` containing the generated bytes.
    *
    * ```js
    * // Asynchronous
    * const {
    *   randomBytes,
    * } = await import('node:crypto');
    *
    * randomBytes(256, (err, buf) => {
    *   if (err) throw err;
    *   console.log(`${buf.length} bytes of random data: ${buf.toString('hex')}`);
    * });
    * ```
    *
    * If the `callback` function is not provided, the random bytes are generated
    * synchronously and returned as a `Buffer`. An error will be thrown if
    * there is a problem generating the bytes.
    *
    * ```js
    * // Synchronous
    * const {
    *   randomBytes,
    * } = await import('node:crypto');
    *
    * const buf = randomBytes(256);
    * console.log(
    *   `${buf.length} bytes of random data: ${buf.toString('hex')}`);
    * ```
    *
    * The `crypto.randomBytes()` method will not complete until there is
    * sufficient entropy available.
    * This should normally never take longer than a few milliseconds. The only time
    * when generating the random bytes may conceivably block for a longer period of
    * time is right after boot, when the whole system is still low on entropy.
    *
    * This API uses libuv's threadpool, which can have surprising and
    * negative performance implications for some applications; see the `UV_THREADPOOL_SIZE` documentation for more information.
    *
    * The asynchronous version of `crypto.randomBytes()` is carried out in a single
    * threadpool request. To minimize threadpool task length variation, partition
    * large `randomBytes` requests when doing so as part of fulfilling a client
    * request.
    * @since v0.5.8
    * @param size The number of bytes to generate. The `size` must not be larger than `2**31 - 1`.
    * @return if the `callback` function is not provided.
    */
  inline def randomBytes(size: Double): Buffer = ^.asInstanceOf[js.Dynamic].applyDynamic("randomBytes")(size.asInstanceOf[js.Any]).asInstanceOf[Buffer]
  inline def randomBytes(size: Double, callback: js.Function2[/* err */ js.Error | Null, /* buf */ Buffer, Unit]): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomBytes")(size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  inline def randomFill(
    buffer: js.typedarray.DataView,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.DataView, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.DataView,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.DataView, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.DataView,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.DataView, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Float32Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Float32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Float32Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Float32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Float32Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Float32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Float64Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Float64Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Float64Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Float64Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Float64Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Float64Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int16Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int16Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int16Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int16Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int16Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int16Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int32Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int32Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int32Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int8Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int8Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int8Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int8Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Int8Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Int8Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint16Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint16Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint16Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint16Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint16Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint16Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint32Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint32Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint32Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint32Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  /**
    * This function is similar to {@link randomBytes} but requires the first
    * argument to be a `Buffer` that will be filled. It also
    * requires that a callback is passed in.
    *
    * If the `callback` function is not provided, an error will be thrown.
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const { randomFill } = await import('node:crypto');
    *
    * const buf = Buffer.alloc(10);
    * randomFill(buf, (err, buf) => {
    *   if (err) throw err;
    *   console.log(buf.toString('hex'));
    * });
    *
    * randomFill(buf, 5, (err, buf) => {
    *   if (err) throw err;
    *   console.log(buf.toString('hex'));
    * });
    *
    * // The above is equivalent to the following:
    * randomFill(buf, 5, 5, (err, buf) => {
    *   if (err) throw err;
    *   console.log(buf.toString('hex'));
    * });
    * ```
    *
    * Any `ArrayBuffer`, `TypedArray`, or `DataView` instance may be passed as `buffer`.
    *
    * While this includes instances of `Float32Array` and `Float64Array`, this
    * function should not be used to generate random floating-point numbers. The
    * result may contain `+Infinity`, `-Infinity`, and `NaN`, and even if the array
    * contains finite numbers only, they are not drawn from a uniform random
    * distribution and have no meaningful lower or upper bounds.
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const { randomFill } = await import('node:crypto');
    *
    * const a = new Uint32Array(10);
    * randomFill(a, (err, buf) => {
    *   if (err) throw err;
    *   console.log(Buffer.from(buf.buffer, buf.byteOffset, buf.byteLength)
    *     .toString('hex'));
    * });
    *
    * const b = new DataView(new ArrayBuffer(10));
    * randomFill(b, (err, buf) => {
    *   if (err) throw err;
    *   console.log(Buffer.from(buf.buffer, buf.byteOffset, buf.byteLength)
    *     .toString('hex'));
    * });
    *
    * const c = new ArrayBuffer(10);
    * randomFill(c, (err, buf) => {
    *   if (err) throw err;
    *   console.log(Buffer.from(buf).toString('hex'));
    * });
    * ```
    *
    * This API uses libuv's threadpool, which can have surprising and
    * negative performance implications for some applications; see the `UV_THREADPOOL_SIZE` documentation for more information.
    *
    * The asynchronous version of `crypto.randomFill()` is carried out in a single
    * threadpool request. To minimize threadpool task length variation, partition
    * large `randomFill` requests when doing so as part of fulfilling a client
    * request.
    * @since v7.10.0, v6.13.0
    * @param buffer Must be supplied. The size of the provided `buffer` must not be larger than `2**31 - 1`.
    * @param [offset=0]
    * @param [size=buffer.length - offset]
    * @param callback `function(err, buf) {}`.
    */
  inline def randomFill(
    buffer: js.typedarray.Uint8Array,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint8Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint8Array,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint8Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint8Array,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint8Array, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint8ClampedArray,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint8ClampedArray, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint8ClampedArray,
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint8ClampedArray, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: js.typedarray.Uint8ClampedArray,
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ js.typedarray.Uint8ClampedArray, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: RelativeIndexable[js.BigInt],
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ RelativeIndexable[js.BigInt], Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: RelativeIndexable[js.BigInt],
    offset: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ RelativeIndexable[js.BigInt], Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomFill(
    buffer: RelativeIndexable[js.BigInt],
    offset: Double,
    size: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* buf */ RelativeIndexable[js.BigInt], Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFill")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  inline def randomFillSync(buffer: js.typedarray.DataView): js.typedarray.DataView = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.DataView]
  inline def randomFillSync(buffer: js.typedarray.DataView, offset: Double): js.typedarray.DataView = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.DataView]
  inline def randomFillSync(buffer: js.typedarray.DataView, offset: Double, size: Double): js.typedarray.DataView = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.DataView]
  inline def randomFillSync(buffer: js.typedarray.DataView, offset: Unit, size: Double): js.typedarray.DataView = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.DataView]
  inline def randomFillSync(buffer: js.typedarray.Float32Array): js.typedarray.Float32Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Float32Array]
  inline def randomFillSync(buffer: js.typedarray.Float32Array, offset: Double): js.typedarray.Float32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Float32Array]
  inline def randomFillSync(buffer: js.typedarray.Float32Array, offset: Double, size: Double): js.typedarray.Float32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Float32Array]
  inline def randomFillSync(buffer: js.typedarray.Float32Array, offset: Unit, size: Double): js.typedarray.Float32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Float32Array]
  inline def randomFillSync(buffer: js.typedarray.Float64Array): js.typedarray.Float64Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Float64Array]
  inline def randomFillSync(buffer: js.typedarray.Float64Array, offset: Double): js.typedarray.Float64Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Float64Array]
  inline def randomFillSync(buffer: js.typedarray.Float64Array, offset: Double, size: Double): js.typedarray.Float64Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Float64Array]
  inline def randomFillSync(buffer: js.typedarray.Float64Array, offset: Unit, size: Double): js.typedarray.Float64Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Float64Array]
  inline def randomFillSync(buffer: js.typedarray.Int16Array): js.typedarray.Int16Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Int16Array]
  inline def randomFillSync(buffer: js.typedarray.Int16Array, offset: Double): js.typedarray.Int16Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int16Array]
  inline def randomFillSync(buffer: js.typedarray.Int16Array, offset: Double, size: Double): js.typedarray.Int16Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int16Array]
  inline def randomFillSync(buffer: js.typedarray.Int16Array, offset: Unit, size: Double): js.typedarray.Int16Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int16Array]
  inline def randomFillSync(buffer: js.typedarray.Int32Array): js.typedarray.Int32Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Int32Array]
  inline def randomFillSync(buffer: js.typedarray.Int32Array, offset: Double): js.typedarray.Int32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int32Array]
  inline def randomFillSync(buffer: js.typedarray.Int32Array, offset: Double, size: Double): js.typedarray.Int32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int32Array]
  inline def randomFillSync(buffer: js.typedarray.Int32Array, offset: Unit, size: Double): js.typedarray.Int32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int32Array]
  inline def randomFillSync(buffer: js.typedarray.Int8Array): js.typedarray.Int8Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Int8Array]
  inline def randomFillSync(buffer: js.typedarray.Int8Array, offset: Double): js.typedarray.Int8Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int8Array]
  inline def randomFillSync(buffer: js.typedarray.Int8Array, offset: Double, size: Double): js.typedarray.Int8Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int8Array]
  inline def randomFillSync(buffer: js.typedarray.Int8Array, offset: Unit, size: Double): js.typedarray.Int8Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Int8Array]
  inline def randomFillSync(buffer: js.typedarray.Uint16Array): js.typedarray.Uint16Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Uint16Array]
  inline def randomFillSync(buffer: js.typedarray.Uint16Array, offset: Double): js.typedarray.Uint16Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint16Array]
  inline def randomFillSync(buffer: js.typedarray.Uint16Array, offset: Double, size: Double): js.typedarray.Uint16Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint16Array]
  inline def randomFillSync(buffer: js.typedarray.Uint16Array, offset: Unit, size: Double): js.typedarray.Uint16Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint16Array]
  inline def randomFillSync(buffer: js.typedarray.Uint32Array): js.typedarray.Uint32Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Uint32Array]
  inline def randomFillSync(buffer: js.typedarray.Uint32Array, offset: Double): js.typedarray.Uint32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint32Array]
  inline def randomFillSync(buffer: js.typedarray.Uint32Array, offset: Double, size: Double): js.typedarray.Uint32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint32Array]
  inline def randomFillSync(buffer: js.typedarray.Uint32Array, offset: Unit, size: Double): js.typedarray.Uint32Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint32Array]
  /**
    * Synchronous version of {@link randomFill}.
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const { randomFillSync } = await import('node:crypto');
    *
    * const buf = Buffer.alloc(10);
    * console.log(randomFillSync(buf).toString('hex'));
    *
    * randomFillSync(buf, 5);
    * console.log(buf.toString('hex'));
    *
    * // The above is equivalent to the following:
    * randomFillSync(buf, 5, 5);
    * console.log(buf.toString('hex'));
    * ```
    *
    * Any `ArrayBuffer`, `TypedArray` or `DataView` instance may be passed as`buffer`.
    *
    * ```js
    * import { Buffer } from 'node:buffer';
    * const { randomFillSync } = await import('node:crypto');
    *
    * const a = new Uint32Array(10);
    * console.log(Buffer.from(randomFillSync(a).buffer,
    *                         a.byteOffset, a.byteLength).toString('hex'));
    *
    * const b = new DataView(new ArrayBuffer(10));
    * console.log(Buffer.from(randomFillSync(b).buffer,
    *                         b.byteOffset, b.byteLength).toString('hex'));
    *
    * const c = new ArrayBuffer(10);
    * console.log(Buffer.from(randomFillSync(c)).toString('hex'));
    * ```
    * @since v7.10.0, v6.13.0
    * @param buffer Must be supplied. The size of the provided `buffer` must not be larger than `2**31 - 1`.
    * @param [offset=0]
    * @param [size=buffer.length - offset]
    * @return The object passed as `buffer` argument.
    */
  inline def randomFillSync(buffer: js.typedarray.Uint8Array): js.typedarray.Uint8Array = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Uint8Array]
  inline def randomFillSync(buffer: js.typedarray.Uint8Array, offset: Double): js.typedarray.Uint8Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint8Array]
  inline def randomFillSync(buffer: js.typedarray.Uint8Array, offset: Double, size: Double): js.typedarray.Uint8Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint8Array]
  inline def randomFillSync(buffer: js.typedarray.Uint8Array, offset: Unit, size: Double): js.typedarray.Uint8Array = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint8Array]
  inline def randomFillSync(buffer: js.typedarray.Uint8ClampedArray): js.typedarray.Uint8ClampedArray = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[js.typedarray.Uint8ClampedArray]
  inline def randomFillSync(buffer: js.typedarray.Uint8ClampedArray, offset: Double): js.typedarray.Uint8ClampedArray = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint8ClampedArray]
  inline def randomFillSync(buffer: js.typedarray.Uint8ClampedArray, offset: Double, size: Double): js.typedarray.Uint8ClampedArray = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint8ClampedArray]
  inline def randomFillSync(buffer: js.typedarray.Uint8ClampedArray, offset: Unit, size: Double): js.typedarray.Uint8ClampedArray = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[js.typedarray.Uint8ClampedArray]
  inline def randomFillSync(buffer: RelativeIndexable[js.BigInt]): RelativeIndexable[js.BigInt] = ^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any]).asInstanceOf[RelativeIndexable[js.BigInt]]
  inline def randomFillSync(buffer: RelativeIndexable[js.BigInt], offset: Double): RelativeIndexable[js.BigInt] = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any])).asInstanceOf[RelativeIndexable[js.BigInt]]
  inline def randomFillSync(buffer: RelativeIndexable[js.BigInt], offset: Double, size: Double): RelativeIndexable[js.BigInt] = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[RelativeIndexable[js.BigInt]]
  inline def randomFillSync(buffer: RelativeIndexable[js.BigInt], offset: Unit, size: Double): RelativeIndexable[js.BigInt] = (^.asInstanceOf[js.Dynamic].applyDynamic("randomFillSync")(buffer.asInstanceOf[js.Any], offset.asInstanceOf[js.Any], size.asInstanceOf[js.Any])).asInstanceOf[RelativeIndexable[js.BigInt]]
  
  /**
    * Return a random integer `n` such that `min <= n < max`.  This
    * implementation avoids [modulo bias](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#Modulo_bias).
    *
    * The range (`max - min`) must be less than 2**48. `min` and `max` must
    * be [safe integers](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isSafeInteger).
    *
    * If the `callback` function is not provided, the random integer is
    * generated synchronously.
    *
    * ```js
    * // Asynchronous
    * const {
    *   randomInt,
    * } = await import('node:crypto');
    *
    * randomInt(3, (err, n) => {
    *   if (err) throw err;
    *   console.log(`Random number chosen from (0, 1, 2): ${n}`);
    * });
    * ```
    *
    * ```js
    * // Synchronous
    * const {
    *   randomInt,
    * } = await import('node:crypto');
    *
    * const n = randomInt(3);
    * console.log(`Random number chosen from (0, 1, 2): ${n}`);
    * ```
    *
    * ```js
    * // With `min` argument
    * const {
    *   randomInt,
    * } = await import('node:crypto');
    *
    * const n = randomInt(1, 7);
    * console.log(`The dice rolled: ${n}`);
    * ```
    * @since v14.10.0, v12.19.0
    * @param [min=0] Start of random range (inclusive).
    * @param max End of random range (exclusive).
    * @param callback `function(err, n) {}`.
    */
  inline def randomInt(max: Double): Double = ^.asInstanceOf[js.Dynamic].applyDynamic("randomInt")(max.asInstanceOf[js.Any]).asInstanceOf[Double]
  inline def randomInt(max: Double, callback: js.Function2[/* err */ js.Error | Null, /* value */ Double, Unit]): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomInt")(max.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def randomInt(min: Double, max: Double): Double = (^.asInstanceOf[js.Dynamic].applyDynamic("randomInt")(min.asInstanceOf[js.Any], max.asInstanceOf[js.Any])).asInstanceOf[Double]
  inline def randomInt(
    min: Double,
    max: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* value */ Double, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("randomInt")(min.asInstanceOf[js.Any], max.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Generates a random [RFC 4122](https://www.rfc-editor.org/rfc/rfc4122.txt) version 4 UUID. The UUID is generated using a
    * cryptographic pseudorandom number generator.
    * @since v15.6.0, v14.17.0
    */
  inline def randomUUID(): UUID = ^.asInstanceOf[js.Dynamic].applyDynamic("randomUUID")().asInstanceOf[UUID]
  inline def randomUUID(options: RandomUUIDOptions): UUID = ^.asInstanceOf[js.Dynamic].applyDynamic("randomUUID")(options.asInstanceOf[js.Any]).asInstanceOf[UUID]
  
  /**
    * Provides an asynchronous [scrypt](https://en.wikipedia.org/wiki/Scrypt) implementation. Scrypt is a password-based
    * key derivation function that is designed to be expensive computationally and
    * memory-wise in order to make brute-force attacks unrewarding.
    *
    * The `salt` should be as unique as possible. It is recommended that a salt is
    * random and at least 16 bytes long. See [NIST SP 800-132](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-132.pdf) for details.
    *
    * When passing strings for `password` or `salt`, please consider `caveats when using strings as inputs to cryptographic APIs`.
    *
    * The `callback` function is called with two arguments: `err` and `derivedKey`. `err` is an exception object when key derivation fails, otherwise `err` is `null`. `derivedKey` is passed to the
    * callback as a `Buffer`.
    *
    * An exception is thrown when any of the input arguments specify invalid values
    * or types.
    *
    * ```js
    * const {
    *   scrypt,
    * } = await import('node:crypto');
    *
    * // Using the factory defaults.
    * scrypt('password', 'salt', 64, (err, derivedKey) => {
    *   if (err) throw err;
    *   console.log(derivedKey.toString('hex'));  // '3745e48...08d59ae'
    * });
    * // Using a custom N parameter. Must be a power of two.
    * scrypt('password', 'salt', 64, { N: 1024 }, (err, derivedKey) => {
    *   if (err) throw err;
    *   console.log(derivedKey.toString('hex'));  // '3745e48...aa39b34'
    * });
    * ```
    * @since v10.5.0
    */
  inline def scrypt(
    password: BinaryLike,
    salt: BinaryLike,
    keylen: Double,
    callback: js.Function2[/* err */ js.Error | Null, /* derivedKey */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("scrypt")(password.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def scrypt(
    password: BinaryLike,
    salt: BinaryLike,
    keylen: Double,
    options: ScryptOptions,
    callback: js.Function2[/* err */ js.Error | Null, /* derivedKey */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("scrypt")(password.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], options.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Provides a synchronous [scrypt](https://en.wikipedia.org/wiki/Scrypt) implementation. Scrypt is a password-based
    * key derivation function that is designed to be expensive computationally and
    * memory-wise in order to make brute-force attacks unrewarding.
    *
    * The `salt` should be as unique as possible. It is recommended that a salt is
    * random and at least 16 bytes long. See [NIST SP 800-132](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-132.pdf) for details.
    *
    * When passing strings for `password` or `salt`, please consider `caveats when using strings as inputs to cryptographic APIs`.
    *
    * An exception is thrown when key derivation fails, otherwise the derived key is
    * returned as a `Buffer`.
    *
    * An exception is thrown when any of the input arguments specify invalid values
    * or types.
    *
    * ```js
    * const {
    *   scryptSync,
    * } = await import('node:crypto');
    * // Using the factory defaults.
    *
    * const key1 = scryptSync('password', 'salt', 64);
    * console.log(key1.toString('hex'));  // '3745e48...08d59ae'
    * // Using a custom N parameter. Must be a power of two.
    * const key2 = scryptSync('password', 'salt', 64, { N: 1024 });
    * console.log(key2.toString('hex'));  // '3745e48...aa39b34'
    * ```
    * @since v10.5.0
    */
  inline def scryptSync(password: BinaryLike, salt: BinaryLike, keylen: Double): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("scryptSync")(password.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def scryptSync(password: BinaryLike, salt: BinaryLike, keylen: Double, options: ScryptOptions): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("scryptSync")(password.asInstanceOf[js.Any], salt.asInstanceOf[js.Any], keylen.asInstanceOf[js.Any], options.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  
  /**
    * @since v15.6.0
    */
  inline def secureHeapUsed(): SecureHeapUsage = ^.asInstanceOf[js.Dynamic].applyDynamic("secureHeapUsed")().asInstanceOf[SecureHeapUsage]
  
  /**
    * Load and set the `engine` for some or all OpenSSL functions (selected by flags).
    *
    * `engine` could be either an id or a path to the engine's shared library.
    *
    * The optional `flags` argument uses `ENGINE_METHOD_ALL` by default. The `flags` is a bit field taking one of or a mix of the following flags (defined in `crypto.constants`):
    *
    * * `crypto.constants.ENGINE_METHOD_RSA`
    * * `crypto.constants.ENGINE_METHOD_DSA`
    * * `crypto.constants.ENGINE_METHOD_DH`
    * * `crypto.constants.ENGINE_METHOD_RAND`
    * * `crypto.constants.ENGINE_METHOD_EC`
    * * `crypto.constants.ENGINE_METHOD_CIPHERS`
    * * `crypto.constants.ENGINE_METHOD_DIGESTS`
    * * `crypto.constants.ENGINE_METHOD_PKEY_METHS`
    * * `crypto.constants.ENGINE_METHOD_PKEY_ASN1_METHS`
    * * `crypto.constants.ENGINE_METHOD_ALL`
    * * `crypto.constants.ENGINE_METHOD_NONE`
    * @since v0.11.11
    * @param flags
    */
  inline def setEngine(engine: String): Unit = ^.asInstanceOf[js.Dynamic].applyDynamic("setEngine")(engine.asInstanceOf[js.Any]).asInstanceOf[Unit]
  inline def setEngine(engine: String, flags: Double): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("setEngine")(engine.asInstanceOf[js.Any], flags.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * Enables the FIPS compliant crypto provider in a FIPS-enabled Node.js build.
    * Throws an error if FIPS mode is not available.
    * @since v10.0.0
    * @param bool `true` to enable FIPS mode.
    */
  inline def setFips(bool: Boolean): Unit = ^.asInstanceOf[js.Dynamic].applyDynamic("setFips")(bool.asInstanceOf[js.Any]).asInstanceOf[Unit]
  
  /**
    * Calculates and returns the signature for `data` using the given private key and
    * algorithm. If `algorithm` is `null` or `undefined`, then the algorithm is
    * dependent upon the key type (especially Ed25519 and Ed448).
    *
    * If `key` is not a `KeyObject`, this function behaves as if `key` had been
    * passed to {@link createPrivateKey}. If it is an object, the following
    * additional properties can be passed:
    *
    * If the `callback` function is provided this function uses libuv's threadpool.
    * @since v12.0.0
    */
  inline def sign(algorithm: String, data: ArrayBufferView, key: KeyLike): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: String,
    data: ArrayBufferView,
    key: KeyLike,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: String, data: ArrayBufferView, key: SignJsonWebKeyInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: String,
    data: ArrayBufferView,
    key: SignJsonWebKeyInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: String, data: ArrayBufferView, key: SignKeyObjectInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: String,
    data: ArrayBufferView,
    key: SignKeyObjectInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: String, data: ArrayBufferView, key: SignPrivateKeyInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: String,
    data: ArrayBufferView,
    key: SignPrivateKeyInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Null, data: ArrayBufferView, key: KeyLike): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Null,
    data: ArrayBufferView,
    key: KeyLike,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Null, data: ArrayBufferView, key: SignJsonWebKeyInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Null,
    data: ArrayBufferView,
    key: SignJsonWebKeyInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Null, data: ArrayBufferView, key: SignKeyObjectInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Null,
    data: ArrayBufferView,
    key: SignKeyObjectInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Null, data: ArrayBufferView, key: SignPrivateKeyInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Null,
    data: ArrayBufferView,
    key: SignPrivateKeyInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Unit, data: ArrayBufferView, key: KeyLike): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Unit,
    data: ArrayBufferView,
    key: KeyLike,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Unit, data: ArrayBufferView, key: SignJsonWebKeyInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Unit,
    data: ArrayBufferView,
    key: SignJsonWebKeyInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Unit, data: ArrayBufferView, key: SignKeyObjectInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Unit,
    data: ArrayBufferView,
    key: SignKeyObjectInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def sign(algorithm: Unit, data: ArrayBufferView, key: SignPrivateKeyInput): Buffer = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any])).asInstanceOf[Buffer]
  inline def sign(
    algorithm: Unit,
    data: ArrayBufferView,
    key: SignPrivateKeyInput,
    callback: js.Function2[/* error */ js.Error | Null, /* data */ Buffer, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("sign")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  /**
    * A convenient alias for `crypto.webcrypto.subtle`.
    * @since v17.4.0
    */
  @JSImport("node:crypto", "subtle")
  @js.native
  val subtle: SubtleCrypto = js.native
  
  /**
    * This function compares the underlying bytes that represent the given `ArrayBuffer`, `TypedArray`, or `DataView` instances using a constant-time
    * algorithm.
    *
    * This function does not leak timing information that
    * would allow an attacker to guess one of the values. This is suitable for
    * comparing HMAC digests or secret values like authentication cookies or [capability urls](https://www.w3.org/TR/capability-urls/).
    *
    * `a` and `b` must both be `Buffer`s, `TypedArray`s, or `DataView`s, and they
    * must have the same byte length. An error is thrown if `a` and `b` have
    * different byte lengths.
    *
    * If at least one of `a` and `b` is a `TypedArray` with more than one byte per
    * entry, such as `Uint16Array`, the result will be computed using the platform
    * byte order.
    *
    * **When both of the inputs are `Float32Array`s or `Float64Array`s, this function might return unexpected results due to IEEE 754**
    * **encoding of floating-point numbers. In particular, neither `x === y` nor `Object.is(x, y)` implies that the byte representations of two floating-point**
    * **numbers `x` and `y` are equal.**
    *
    * Use of `crypto.timingSafeEqual` does not guarantee that the _surrounding_ code
    * is timing-safe. Care should be taken to ensure that the surrounding code does
    * not introduce timing vulnerabilities.
    * @since v6.6.0
    */
  inline def timingSafeEqual(a: ArrayBufferView, b: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("timingSafeEqual")(a.asInstanceOf[js.Any], b.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  
  /**
    * Verifies the given signature for `data` using the given key and algorithm. If `algorithm` is `null` or `undefined`, then the algorithm is dependent upon the
    * key type (especially Ed25519 and Ed448).
    *
    * If `key` is not a `KeyObject`, this function behaves as if `key` had been
    * passed to {@link createPublicKey}. If it is an object, the following
    * additional properties can be passed:
    *
    * The `signature` argument is the previously calculated signature for the `data`.
    *
    * Because public keys can be derived from private keys, a private key or a public
    * key may be passed for `key`.
    *
    * If the `callback` function is provided this function uses libuv's threadpool.
    * @since v12.0.0
    */
  inline def verify(algorithm: String, data: ArrayBufferView, key: KeyLike, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: String,
    data: ArrayBufferView,
    key: KeyLike,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: String, data: ArrayBufferView, key: VerifyJsonWebKeyInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: String,
    data: ArrayBufferView,
    key: VerifyJsonWebKeyInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: String, data: ArrayBufferView, key: VerifyKeyObjectInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: String,
    data: ArrayBufferView,
    key: VerifyKeyObjectInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: String, data: ArrayBufferView, key: VerifyPublicKeyInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: String,
    data: ArrayBufferView,
    key: VerifyPublicKeyInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Null, data: ArrayBufferView, key: KeyLike, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Null,
    data: ArrayBufferView,
    key: KeyLike,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Null, data: ArrayBufferView, key: VerifyJsonWebKeyInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Null,
    data: ArrayBufferView,
    key: VerifyJsonWebKeyInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Null, data: ArrayBufferView, key: VerifyKeyObjectInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Null,
    data: ArrayBufferView,
    key: VerifyKeyObjectInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Null, data: ArrayBufferView, key: VerifyPublicKeyInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Null,
    data: ArrayBufferView,
    key: VerifyPublicKeyInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Unit, data: ArrayBufferView, key: KeyLike, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Unit,
    data: ArrayBufferView,
    key: KeyLike,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Unit, data: ArrayBufferView, key: VerifyJsonWebKeyInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Unit,
    data: ArrayBufferView,
    key: VerifyJsonWebKeyInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Unit, data: ArrayBufferView, key: VerifyKeyObjectInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Unit,
    data: ArrayBufferView,
    key: VerifyKeyObjectInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  inline def verify(algorithm: Unit, data: ArrayBufferView, key: VerifyPublicKeyInput, signature: ArrayBufferView): Boolean = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any])).asInstanceOf[Boolean]
  inline def verify(
    algorithm: Unit,
    data: ArrayBufferView,
    key: VerifyPublicKeyInput,
    signature: ArrayBufferView,
    callback: js.Function2[/* error */ js.Error | Null, /* result */ Boolean, Unit]
  ): Unit = (^.asInstanceOf[js.Dynamic].applyDynamic("verify")(algorithm.asInstanceOf[js.Any], data.asInstanceOf[js.Any], key.asInstanceOf[js.Any], signature.asInstanceOf[js.Any], callback.asInstanceOf[js.Any])).asInstanceOf[Unit]
  
  @JSImport("node:crypto", "webcrypto")
  @js.native
  val webcrypto: Crypto = js.native
}
