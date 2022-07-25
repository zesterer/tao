curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
wasm-pack build --target bundler --out-dir www/dist
cd www
npm run build
cd ..
