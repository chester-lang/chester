import Link from 'next/link';

export const dynamic = 'force-static';

type Feature = {
  title: string;
  body: string;
};

const features: Feature[] = [
  {
    title: 'Latest React + Next.js',
    body: 'Running on React 19 and Next.js 16 with app router defaults, server components, and the newest rendering pipeline.'
  },
  {
    title: 'Typed by default',
    body: 'Strict TypeScript 5.9 config with modern bundler resolution, so imports stay clean and errors surface early.'
  },
  {
    title: 'pnpm-first workflow',
    body: 'Lean install, deterministic lockfile, and fast dev server spins up with a single command.'
  },
  {
    title: 'Scala.js optional',
    body: 'The marketing site lives entirely in Next.js—no Scala.js dependency unless you want to embed widgets later.'
  }
];

const commands = ['pnpm install --frozen-lockfile', 'pnpm dev', 'pnpm build && pnpm start'];

export default function HomePage() {
  return (
    <>
      <section className="hero">
        <div className="hero-copy">
          <div className="pill">
            Chester website · <strong>React 19 + Next.js 16</strong>
          </div>
          <h1>New Chester site, purpose-built for speed.</h1>
          <p>
            A clean, TypeScript-first Next.js surface that keeps Scala.js out of the critical path. Iterate fast, publish
            faster, and keep the language story front and center.
          </p>
          <div className="cta">
            <Link className="btn primary" href="https://github.com/chester-lang">
              View Chester on GitHub →
            </Link>
            <Link className="btn secondary" href="https://nextjs.org/docs" target="_blank" rel="noreferrer">
              Next.js docs
            </Link>
          </div>
        </div>

        <div className="panel">
          <img alt="Chester logomark" className="logo" src="/chester-logo.svg" />
          <h3>Up and running in seconds</h3>
          <p>Use pnpm everywhere. No extra sbt wiring is needed to work on the site.</p>
          <ul className="stack-list">
            {commands.map((cmd) => (
              <li key={cmd} className="stack-item">
                <span>›</span>
                <strong>{cmd}</strong>
              </li>
            ))}
          </ul>
          <div className="code-block">
            <code>pnpm dev</code>
          </div>
        </div>
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
        <div>Built with pnpm, ready for Vercel/Node 18+.</div>
        <div className="tags">
          <span className="tag">Next 16.0.8</span>
          <span className="tag">React 19.2.1</span>
          <span className="tag">TypeScript 5.9</span>
          <span className="tag">pnpm 10</span>
        </div>
      </footer>
    </>
  );
}
