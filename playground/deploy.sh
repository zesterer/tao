git submodule init &&
git submodule update &&
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain nightly -y &&
source $HOME/.cargo/env &&
cd playground &&
rustup override set nightly &&
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh &&
npm install &&
npm run build
