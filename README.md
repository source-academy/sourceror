
# Sourceror

Sourceror is a compiler from Source to WebAssembly, written in Rust.  For deployment, the compiler is itself compiled to WebAssembly using the existing Rust toolchain.  It is also possible to compile the compiler to the native binary format and architecture (e.g. x86), which is useful for debugging.

Note: If you just want to play with it and don't want to spend time building it on your own, you can go to [Source Academy](https://source-academy.github.io/) and switch the language to "Source §1 WebAssembly".  Your Source programs will be compiled and run in your browser.

## Usage

Sourceror should be used with [Sourceror Driver](https://github.com/source-academy/sourceror-driver).  On its own, Sourceror only compiles a validated ESTree to WebAssembly.  Sourceror Driver invokes js-slang to convert Source to a validated ESTree, and then invokes Sourceror for the rest of the compilation process.  Sourceror Driver also includes functionality to run the compiled WebAssembly binary.

All the commands below assume that you have an up-to-date installation of the Rust compiler toolchain.

To build, navigate to the root directory of this repository, and do

```
npm run build
```

The `/dist` folder will be created and it will contain `package.json`, `index.js`, and a `sourceror` folder.  The `/dist` folder is the package root for modules that depend on Sourceror (such as Sourceror Driver).  The `sourceror` folder should be copied to the `externalLibs` folder of your web server.

We are actually building a separate bundle that can be consumed by another bundler &mdash; this is because older bundlers (such as the one used in cadet-frontend) do not know how to bundle a WebAssembly binary.

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
