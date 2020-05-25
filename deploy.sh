git submodule init &&
git submodule update &&
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain nightly -y &&
cd playground &&
rustup override set nightly &&
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh &&
npm run build
