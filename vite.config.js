import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import { fileURLToPath, URL } from 'node:url'

// https://vite.dev/config/
export default defineConfig({
  plugins: [vue()],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url))
    }
  },
  server: {
    proxy: {
      '/api': 'http://api-server:8000/'
    }
  },
  test:{
    globals: true,
    environment: 'jsdom',
    alias: [
      {
        find: /^monaco-editor$/,
        replacement: __dirname + '/node_modules/monaco-editor/esm/vs/editor/editor.api'
      }
    ],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'html'],
      all: true,
      include: ['src/**/*.{js,ts,vue}'],
      exclude: ['node_modules/', 'tests/'],
    },
  }
})
