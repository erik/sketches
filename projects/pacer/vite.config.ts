import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  resolve: {
    alias: [{ find: "@", replacement: "/src" }],
  },
  plugins: [tailwindcss()],
  esbuild: {
    jsx: "transform",
    jsxDev: false,
    jsxFragment: "createFragment",
    jsxInject: `import { createElement, createFragment } from '@/livewire.js'`,
    jsxFactory: "createElement",
  },
});
