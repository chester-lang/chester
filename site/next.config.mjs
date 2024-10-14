import createNextIntlPlugin from 'next-intl/plugin';
import path from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';

const withNextIntl = createNextIntlPlugin('./app/request.ts');

// Get the directory path of the current module
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const from = path.join(__dirname, '..', 'resources', 'favicon.ico');
const to = path.join(__dirname, 'app', 'favicon.ico');
if (fs.existsSync(from)) {
    fs.copyFileSync(from, to);
    console.log('Favicon copied successfully');
} else {
    console.warn('Favicon not found in resources directory');
}
/** @type {import('next').NextConfig} */
const nextConfig = {
    /**
     * Enable static exports for the App Router.
     *
     * @see https://nextjs.org/docs/app/building-your-application/deploying/static-exports
     */
    output: "export",

    /**
     * Set base path. This is the slug of your GitHub repository.
     *
     * @see https://nextjs.org/docs/app/api-reference/next-config-js/basePath
     */
    basePath: "",

    /**
     * Disable server-based image optimization. Next.js does not support
     * dynamic features with static exports.
     *
     * @see https://nextjs.org/docs/app/api-reference/components/image#unoptimized
     */
    images: {
        unoptimized: true,
    },
}

export default withNextIntl(nextConfig);
