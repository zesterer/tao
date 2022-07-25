curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
wasm-pack build --target bundler --out-dir site/dist
cd site
npm install --save-dev webpack
npm install
npm run build
cd ..
