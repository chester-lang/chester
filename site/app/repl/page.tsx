'use client';

import type { Terminal } from '@xterm/xterm';
import type { FitAddon } from '@xterm/addon-fit';
import { useEffect, useRef, useState } from 'react';
import '@xterm/xterm/css/xterm.css';

export const dynamic = 'force-static';
export const ssr = false;

type ReplCallbacks = {
  readLine(prompt: string): Promise<string | null | undefined>;
  print(line: string, isError?: boolean): void;
};

const createReadline = (terminal: Terminal) => (prompt: string): Promise<string | null> =>
  new Promise((resolve) => {
    let buffer = '';
    terminal.write(prompt);

    const disposable = terminal.onData((data) => {
      switch (data) {
        case '\u0003': // ctrl+c
          terminal.writeln('^C');
          disposable.dispose();
          resolve(null);
          return;
        case '\r':
        case '\n':
          terminal.writeln('');
          disposable.dispose();
          resolve(buffer);
          return;
        case '\u007F': // backspace
          if (buffer.length > 0) {
            buffer = buffer.slice(0, -1);
            terminal.write('\b \b');
          }
          return;
        default:
          buffer += data;
          terminal.write(data);
      }
    });
  });

export default function ReplPage() {
  const termRef = useRef<Terminal | null>(null);
  const containerRef = useRef<HTMLDivElement | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    let disposed = false;
    let fitAddon: FitAddon | null = null;

    const start = async () => {
      let terminal: Terminal | null = null;
      try {
        const [{ Terminal }, { FitAddon }] = await Promise.all([
          import('@xterm/xterm') as Promise<typeof import('@xterm/xterm')>,
          import('@xterm/addon-fit') as Promise<typeof import('@xterm/addon-fit')>
        ]);

        if (disposed) return;

        const terminal = new Terminal({
          convertEol: true,
          cursorBlink: true,
          disableStdin: false,
          fontFamily: 'JetBrains Mono, SFMono-Regular, ui-monospace, Menlo, Monaco, Consolas, monospace',
          theme: {
            background: '#0c0f1a',
            foreground: '#e8edf5',
            cursor: '#7ce7ff'
          }
        });
        fitAddon = new FitAddon();
        terminal.loadAddon(fitAddon);
        terminal.open(container);
        fitAddon.fit();
        termRef.current = terminal;

        const callbacks: ReplCallbacks = {
          readLine: createReadline(terminal),
          print: (line, isError = false) => {
            if (isError) terminal.writeln(`\u001b[31m${line}\u001b[0m`);
            else terminal.writeln(line);
          }
        };

        const module: any = await import(
          /* webpackIgnore: true */
          new URL('/scala/web-repl.js', window.location.href).toString()
        );
        if (!module?.startRepl) {
          throw new Error('startRepl not found. Run `sbt webRepl/copyWebRepl` first.');
        }
        await module.startRepl(callbacks);
      } catch (e) {
        const message = e instanceof Error ? e.message : String(e);
        setError(message);
        if (terminal) {
          (terminal as any).writeln(`\u001b[31mREPL failed: ${message}\u001b[0m`);
        }
      }
    };

    start();

    const onResize = () => {
      if (fitAddon) fitAddon.fit();
    };
    window.addEventListener('resize', onResize);

    return () => {
      window.removeEventListener('resize', onResize);
      disposed = true;
      termRef.current?.dispose();
    };
  }, []);

  return (
    <>
      <div className="pill">REPL · browser-hosted · powered by CLI logic</div>
      <h1>Chester REPL</h1>
      <p style={{ color: 'var(--muted)', maxWidth: 720 }}>
        This REPL reuses the shared CLI logic compiled to Scala.js. Build the bundle once with
        <code style={{ padding: '0 8px' }}>sbt webRepl/copyWebRepl</code>, then the browser loads <code>/scala/web-repl.js</code>.
      </p>
      <div className="terminal-wrapper">
        <div className="terminal-header">
          <span>chester&nbsp;· REPL</span>
          <span style={{ color: 'var(--muted)', fontSize: 14 }}>CTRL+C clears the current line</span>
        </div>
        <div className="terminal-body" ref={containerRef} />
      </div>
      {error ? (
        <div className="panel" style={{ marginTop: 16 }}>
          <h3>REPL not ready</h3>
          <p>{error}</p>
          <p>Make sure you ran <code>sbt webRepl/copyWebRepl</code> before starting Next.js.</p>
        </div>
      ) : null}
    </>
  );
}
