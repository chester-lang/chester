export type ReplCallbacks = {
  readLine(prompt: string): Promise<string | null | undefined>;
  print(line: string, isError?: boolean): void;
};

export function startRepl(callbacks: ReplCallbacks): Promise<void>;
