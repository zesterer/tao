curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
wasm-pack build --target bundler --out-dir site/dist
cd site
npm run build
cd ..
