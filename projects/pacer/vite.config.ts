import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

// Custom plugin to inject JSX runtime only into JSX/TSX files
function livewireJsxPlugin() {
  return {
    name: "livewireJsxPlugin",
    transform(code: string, id: string) {
      if (!id.endsWith(".jsx") && !id.endsWith(".tsx")) {
        return null;
      }

      const injectCode = `import { createElement, createFragment } from '@/livewire';\n`;
      return {
        code: injectCode + code,
        map: null,
      };
    },
  };
}

export default defineConfig({
  resolve: {
    alias: [{ find: "@", replacement: "/src" }],
  },
  plugins: [tailwindcss(), livewireJsxPlugin()],
  oxc: {
    jsx: {
      runtime: "classic",
      development: false,
      pragma: "createElement",
      pragmaFrag: "createFragment",
    },
  },
});
