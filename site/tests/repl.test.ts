import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import { expect, test } from 'vitest';
import type { ReplCallbacks } from '../scala/web-repl';

test('web repl bundle exports startRepl and runs with mocked callbacks', async () => {
  const bundlePath = path.resolve(__dirname, '../public/scala/web-repl.js');
  expect(fs.existsSync(bundlePath)).toBe(true);

  const module = await import(pathToFileURL(bundlePath).href);
  expect(typeof module.startRepl).toBe('function');

  const printed: { line: string; isError: boolean }[] = [];
  const responses = [':quit'];

  const callbacks: ReplCallbacks = {
    readLine(prompt) {
      printed.push({ line: prompt, isError: false });
      const next = responses.shift();
      return Promise.resolve(next ?? null);
    },
    print(line, isError = false) {
      printed.push({ line, isError: Boolean(isError) });
    }
  };

  await module.startRepl(callbacks);

  expect(printed.some((p) => p.line.includes('Goodbye.'))).toBe(true);
});
