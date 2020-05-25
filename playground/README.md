# Tao Playground

The tao playground implementation using wasm

## Building

- Clone the repo
- Initialize the submodules
    ```
    $ git submodule init
    $ git submodule update
    ```
- Change the working directory to `playground/`
    ```
    cd playground
    ```
- Install [rustup](https://rustup.rs/)
- Set the toolchain to nightly:
    ```
    $ rustup override set nightly
    ```
- Install [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/)
- Run `npm run build`
- The `dist/` directory will contain all the files 

## Developing

First follow all steps in [Building](#Building) and run `npm start` this will start a webpack dev server