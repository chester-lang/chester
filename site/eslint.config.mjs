import next from 'eslint-config-next';

export default [
  ...next,
  {
    ignores: ['.next/**', 'node_modules/**'],
    rules: {
      'react/jsx-sort-props': ['error', { reservedFirst: true }]
    }
  }
];
