import type { Metadata } from 'next';
import type { ReactNode } from 'react';
import './globals.css';

export const metadata: Metadata = {
  title: 'Chester • Modern, fast, typed',
  description: 'Chester in 2025: a crisp Next.js front-end with minimal Scala.js glue.'
};

export default function RootLayout({ children }: { children: ReactNode }) {
  return (
    <html lang="en">
      <body>
        <div className="main-shell">{children}</div>
      </body>
    </html>
  );
}
