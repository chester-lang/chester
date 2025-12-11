export const dynamic = 'force-static';
import ReplTerminal from './ReplTerminal';

export default function ReplPage() {
  return (
    <>
      <div className="pill">REPL · browser-hosted · powered by CLI logic</div>
      <h1>Chester REPL</h1>
      <p style={{ color: 'var(--muted)', maxWidth: 720 }}>
        This REPL reuses the shared CLI logic compiled to Scala.js. Build the bundle once with
        <code style={{ padding: '0 8px' }}>sbt webRepl/copyWebRepl</code>, then the browser loads <code>/scala/web-repl.js</code>.
      </p>
      <ReplTerminal />
    </>
  );
}
