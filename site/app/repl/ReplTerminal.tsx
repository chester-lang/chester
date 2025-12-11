'use client';

import type { FitAddon } from '@xterm/addon-fit';
import type { Terminal } from '@xterm/xterm';
import { useEffect, useRef, useState } from 'react';
import '@xterm/xterm/css/xterm.css';
import type { ReplCallbacks } from '@/scala/web-repl';

type ReplTerminalProps = {
  compact?: boolean;
  className?: string;
};

const createReadline =
  (terminal: Terminal): ReplCallbacks['readLine'] =>
  (prompt: string): Promise<string | null> =>
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

export default function ReplTerminal({ compact = false, className }: ReplTerminalProps) {
  const containerRef = useRef<HTMLDivElement | null>(null);
  const termRef = useRef<Terminal | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    let disposed = false;
    let fitAddon: FitAddon | null = null;
    let terminal: Terminal | null = null;

    const start = async () => {
      try {
        const [{ Terminal }, { FitAddon }] = await Promise.all([
          import('@xterm/xterm') as Promise<typeof import('@xterm/xterm')>,
          import('@xterm/addon-fit') as Promise<typeof import('@xterm/addon-fit')>
        ]);

        if (disposed) return;

        terminal = new Terminal({
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
            if (isError) terminal!.writeln(`\u001b[31m${line}\u001b[0m`);
            else terminal!.writeln(line);
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
          terminal.writeln(`\u001b[31mREPL failed: ${message}\u001b[0m`);
        }
      }
    };

    start();

    const onResize = () => {
      if (fitAddon) fitAddon.fit();
    };
    window.addEventListener('resize', onResize);

    return () => {
      disposed = true;
      window.removeEventListener('resize', onResize);
      termRef.current?.dispose();
    };
  }, []);

  const wrapperClass = ['terminal-wrapper', compact ? 'terminal-wrapper--compact' : '', className]
    .filter(Boolean)
    .join(' ');

  return (
    <>
      <div className={wrapperClass}>
        <div className="terminal-header">
          <span>chester · REPL</span>
          <span className="terminal-hint">CTRL+C clears the current line</span>
        </div>
        <div className="terminal-body" ref={containerRef} />
      </div>
      {error ? (
        <div className="panel" style={{ marginTop: 16 }}>
          <h3>REPL not ready</h3>
          <p>{error}</p>
          <p>
            Make sure you ran <code>sbt webRepl/copyWebRepl</code> before starting Next.js.
          </p>
        </div>
      ) : null}
    </>
  );
}
