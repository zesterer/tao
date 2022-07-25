curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
wasm-pack build --target bundler
cd site
npm install --save-dev webpack
npm run build
cd ..
cp -r site/examples site/dist/examples
