cd web_demo
mkdir site/dist
cp -r site/*.html site/dist/.
cp -r site/*.js site/dist/.
cp -r site/*.css site/dist/.
cp -r site/examples site/dist/examples
cp site/style.css site/dist/style.css
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
wasm-pack build --target bundler
cd site
npm install --save-dev webpack
cd ..
cp -r pkg/* site/dist/*
mkdir site/dist/node_modules
cp -r site/node_modules/@codemirror site/dist/node_modules/.
cp -r site/node_modules/codemirror site/dist/node_modules/.
cp -r site/node_modules/tao_web_demo site/dist/node_modules/.
cd site
npm run build
cd ..
