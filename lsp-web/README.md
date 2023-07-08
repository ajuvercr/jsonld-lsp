# create-svelte

Everything you need to build a Svelte project, powered by [`create-svelte`](https://github.com/sveltejs/kit/tree/master/packages/create-svelte).


## Developing

Once you've created a project and installed dependencies with `npm install` (or `pnpm install` or `yarn`)

Compile the LSP to WASM:

```bash
wasm-pack build -t web --no-default-features
```

Start a development server:


```bash
npm run dev

# or start the server and open the app in a new browser tab
npm run dev -- --open
```

## Building

To create a production version of your app:

```bash
npm run build
```

You can preview the production build with `npm run preview`.

