import Link from 'next/link';
import ReplTerminal from './repl/ReplTerminal';

export const dynamic = 'force-static';

type Feature = {
  title: string;
  body: string;
};

type ContextCard = {
  title: string;
  body: string;
};

type SetupCommand = {
  label: string;
  cmd: string;
};

const features: Feature[] = [
  {
    title: 'Effect-aware core',
    body: 'Functions, records, enums, and Pi types stay explicit about effects with an optional CPS rewrite when you want it.'
  },
  {
    title: 'Readable TypeScript output',
    body: 'Lower a Chester program into a small, predictable TypeScript AST that works for codegen experiments or direct interop.'
  },
  {
    title: 'One pipeline everywhere',
    body: 'The CLI, LSP, and browser REPL all run on the same Scala pipeline—no separate “web mode” to maintain.'
  },
  {
    title: 'Practical ergonomics',
    body: 'A small language surface that favors data-first code and preserves intent when moved between tools.'
  }
];

const context: ContextCard[] = [
  {
    title: 'What is Chester?',
    body: 'A typed language with an effects story and a built-in TypeScript backend, implemented in Scala with both JVM and Scala.js targets.'
  },
  {
    title: 'Why this site?',
    body: 'Showcase the language, ship a browser REPL backed by the real CLI bundle, and keep the getting-started story lightweight.'
  },
  {
    title: 'How to try it locally',
    body: 'Build the Scala.js bundle with sbt webRepl/copyWebRepl, then run pnpm dev. The same bundle powers /repl and the inline REPL below.'
  }
];

const setupCommands: SetupCommand[] = [
  { label: 'Install site deps', cmd: 'pnpm install --frozen-lockfile' },
  { label: 'Build the REPL bundle', cmd: 'sbt webRepl/copyWebRepl' },
  { label: 'Run the site', cmd: 'pnpm dev' }
];

export default function HomePage() {
  return (
    <>
      <section className="hero">
        <div className="hero-copy">
          <div className="pill">
            Chester · <strong>typed language + tooling</strong>
          </div>
          <h1>Typed programs that stay honest about effects and ship as TypeScript.</h1>
          <p>
            Chester keeps the language small but the tooling serious: one Scala pipeline lowers to TypeScript, powers the CLI
            and LSP, and can live inside the browser through a shared Scala.js bundle.
          </p>
          <div className="stat-grid">
            <div className="stat-card">
              <span className="stat-label">TypeScript backend</span>
              <p>Records, enums, functions, and effects lower into a predictable TS AST.</p>
            </div>
            <div className="stat-card">
              <span className="stat-label">Effect story</span>
              <p>Optional CPS rewrite keeps IO explicit when you need it.</p>
            </div>
          </div>
          <div className="cta">
            <Link className="btn primary" href="https://github.com/chester-lang">
              View Chester on GitHub →
            </Link>
            <Link className="btn secondary" href="/repl">
              Open full REPL
            </Link>
            <Link className="btn secondary" href="/docs/">
              Read Documentation
            </Link>
          </div>
        </div>

        <div className="panel hero-panel">
          <div className="hero-panel__header">
            <div className="pill">Browser REPL</div>
            <span className="muted">Same Scala.js bundle as the CLI</span>
          </div>
          <p className="hero-panel__copy">
            Build once with <code>sbt webRepl/copyWebRepl</code> and the site will serve <code>/scala/web-repl.js</code> to power
            the REPL inline.
          </p>
          <ReplTerminal compact />
          <ul className="stack-list hero-stack">
            {setupCommands.map((step) => (
              <li key={step.cmd} className="stack-item">
                <div className="stack-item__label">{step.label}</div>
                <code>{step.cmd}</code>
              </li>
            ))}
          </ul>
        </div>
      </section>

      <section className="context-grid">
        {context.map((item) => (
          <article key={item.title} className="context-card">
            <h3>{item.title}</h3>
            <p>{item.body}</p>
          </article>
        ))}
      </section>

      <section className="feature-grid">
        {features.map((feature) => (
          <article key={feature.title} className="feature-card">
            <span className="feature-dot" aria-hidden />
            <div>
              <h4>{feature.title}</h4>
              <p>{feature.body}</p>
            </div>
          </article>
        ))}
      </section>

      <footer className="footer">
        <div>Shared Scala pipeline powering CLI, LSP, and this REPL.</div>
        <div className="tags">
          <span className="tag">Next 16.0.8</span>
          <span className="tag">React 19.2.1</span>
          <span className="tag">TypeScript 5.9</span>
          <span className="tag">Scala.js bundle for the REPL</span>
        </div>
      </footer>
    </>
  );
}
