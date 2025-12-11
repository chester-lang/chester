# chester

previous version: https://github.com/chester-lang/chester2025draft currently website and ide plugins are for the previous version

```
sbt --batch "cliJVM/runMain chester.cli.Main ts prelude --output prelude/ts-out"

```

## Website (Next.js + pnpm)

The marketing site lives in `site/` and uses the latest Next.js and React stack.

```
cd site
pnpm install
pnpm dev   # start the dev server
pnpm build # production build
```

You can also run the same commands from sbt:

```
sbt "site/pnpmDev"   # dev server
sbt "site/pnpmBuild" # production build
```

### REPL bundle (shared CLI logic)

The browser REPL reuses the CLI. Build the Scala.js bundle and copy it into `site/public/scala`:

```
sbt webRepl/copyWebRepl
```

Then run the site (`pnpm dev`) and open `/repl`. If the bundle is missing, the page will show an error with the command to build it.

To run the site tests (including the REPL mock), build the bundle first and then:

```
cd site
pnpm test
```
