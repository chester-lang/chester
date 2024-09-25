'use client';

import { useState, useEffect, useRef } from 'react';
import { useTranslations } from 'next-intl';
import MonacoEditor from '@/components/MonacoEditor';
import * as monaco from 'monaco-editor';
import { useDebouncedCallback } from 'use-debounce';
import { deflate, inflate } from 'pako';
import { encode as base64Encode, decode as base64Decode } from 'base64-arraybuffer';

export default function PlaygroundPage() {
    const t = useTranslations('PlaygroundPage');
    const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

    // State to hold the current code
    const [code, setCode] = useState('// Write your Chester code here');

    // Encode code to base64 and update the URL hash
    const updateUrlHash = (code: string) => {
        if (typeof window !== 'undefined') {
            const compressed = deflate(code);
            const encodedCode = base64Encode(compressed.buffer);
            window.location.hash = encodedCode;
        }
    };

    // Create a debounced version of updateUrlHash
    const debouncedUpdateUrlHash = useDebouncedCallback((code: string) => {
        updateUrlHash(code);
    }, 500);

    // Decode code from URL hash
    const getCodeFromUrlHash = () => {
        if (typeof window === 'undefined') {
            return null;
        }
        const hash = window.location.hash.substring(1);
        if (hash) {
            try {
                const compressedBuffer = base64Decode(hash);
                const compressed = new Uint8Array(compressedBuffer);
                const decompressed = inflate(compressed, { to: 'string' });
                return decompressed;
            } catch (e) {
                console.error('Error decompressing code from URL hash:', e);
            }
        }
        return null;
    };

    // Update code state with the code from the URL hash on client-side
    useEffect(() => {
        const codeFromHash = getCodeFromUrlHash();
        if (codeFromHash) {
            setCode(codeFromHash);
        }
    }, []);

    // Prevent scroll on hash change
    useEffect(() => {
        const preventScrollOnHashChange = () => {
            window.scrollTo(0, 0);
        };

        window.addEventListener('hashchange', preventScrollOnHashChange);

        return () => {
            window.removeEventListener('hashchange', preventScrollOnHashChange);
        };
    }, []);

    function handleEditorDidMount(editor: monaco.editor.IStandaloneCodeEditor) {
        editorRef.current = editor;
    }

    function handleEditorChange(value?: string) {
        if (value !== undefined) {
            setCode(value);
            debouncedUpdateUrlHash(value);
        }
    }

    function runCode() {
        if (editorRef.current) {
            const code = editorRef.current.getValue();
            // Here you would typically send the code to a backend for execution
            // For now, we'll just display the code in the output area
            const outputElement = document.getElementById('output');
            if (outputElement) {
                outputElement.textContent = `Running code:\n\n${code}`;
            }
        }
    }

    return (
        <div className="flex flex-col min-h-screen">
            <div className="flex-grow flex flex-col items-center justify-center p-4 pb-8 gap-8 sm:p-8 font-[family-name:var(--font-geist-sans)]">
                <main className="flex flex-col gap-6 w-full max-w-4xl">
                    <h1 className="text-2xl font-bold text-center">{t('title')}</h1>
                    <div className="flex flex-col md:flex-row gap-4">
                        <div className="w-full md:w-1/2">
                            <MonacoEditor
                                code={code}
                                onMount={handleEditorDidMount}
                                onChange={handleEditorChange}
                            />
                        </div>
                        <div className="w-full md:w-1/2 bg-black text-white p-4 rounded">
                            <h2 className="text-xl font-bold mb-2">{t('output')}</h2>
                            <pre id="output" className="whitespace-pre-wrap"></pre>
                        </div>
                    </div>
                    <button
                        className="bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded"
                        onClick={runCode}
                    >
                        {t('runCode')}
                    </button>
                </main>
            </div>
        </div>
    );
}