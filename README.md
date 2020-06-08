
# Sourceror

Sourceror is a compiler from Source to WebAssembly, written in Rust.  For deployment, the compiler is itself compiled to WebAssembly using the existing Rust toolchain.  It is also possible to compile the compiler to the native binary format and architecture (e.g. x86), which is useful for debugging.

Note: If you just want to play with it and don't want to spend time building it on your own, you can go to [Source Academy](https://source-academy.github.io/) and switch the language to "Source §1 WebAssembly".  Your Source programs will be compiled and run in your browser.

## Usage

Sourceror should be used with [Sourceror Driver](sourceror-driver). On its own, Sourceror only compiles a validated ESTree to WebAssembly.  Sourceror Driver invokes js-slang to convert Source to a validated ESTree, and then invokes Sourceror for the rest of the compilation process.  Sourceror Driver also includes functionality to run the compiled WebAssembly binary.

Sourcerer Driver is packaged as an NPM package `sourcerer` that bundles the compiled WebAssembly module.

```
yarn add sourcerer
```

To use in a web project, you need a bundler that can handle loading WebAssembly modules, such as Webpack 4 or above.

To use in a Node project, you will need a version of Node that supports loading WebAssembly as an ES module: `--experimental-wasm-modules`.

```js
import { compile, run } from "sourcerer";
import { createContext } from "js-slang";

function compileAndRun(chapter = 1, code: string) {
  let context = createContext(chapter);
  compile(code, context)
    .then((wasm_module) => run(wasm_module, context))
    .then((result) => console.log(result))
    .catch((err) => console.error(err));
}
```

## Building

All the commands below assume that you have an up-to-date installation of the Rust compiler toolchain.

To build, navigate to the `sourceror-driver` directory of this repository, and do

```
yarn run build
```

`dist/` and `wasm/` will be created in `sourceror-driver`.

You can then use your locally-built `sourcerer-driver` in another project (such as `cadet-frontend`) by running

```
yarn link
```

in the `sourcerer-driver` directory, and then

```
yarn link sourcerer
```

in the root of the other project.

## Native binary

Sourceror can be compiled as a native binary for debugging purposes.  You can do

```
cargo run
```

which will build and run a native binary.  You can debug it with the usual debugging tools for C and C++.

Note that this native binary will only accept ESTree input, and not Source source code.

## Contributing

For minor bugs, you can make a pull request directly.  For larger things and debatable features, please file an issue before spending any substantial amount of time on your feature.

Help is also wanted for various bundling and npm issues:
- The fact that Sourceror and Sourceror Driver are in separate repositories (and separate npm packages) is weird.  It would be good if Sourceror Driver could be merged into Sourceror.
- A CLI build of Sourceror Driver would be nice, so that we can have a Source to WebAssembly compiler runnable from the local machine (probably in Node.js).
