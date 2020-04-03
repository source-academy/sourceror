#!/usr/bin/env node
import { compile, run } from './index'
import * as fs from 'fs';

function compileAndRun(
    chapter = 1,
    code: string
) {
    compile(code, { chapter: chapter }).then(wasm_module => run(wasm_module)).then(result => console.log(result)).catch(err => console.error(err));
}

function main() {
  const opt = require('node-getopt')
    .create([
      ['c', 'chapter=CHAPTER', 'set the Source chapter number (i.e., 1-4)', '1']
    ])
    .bindHelp()
    .setHelp('Usage: sourceror PROGRAM_STRING [OPTION]\n\n[[OPTIONS]]')
    .parseSystem()

  const chapter = parseInt(opt.options.chapter, 10)
    const filename = opt.argv[0]
    const code = fs.readFileSync(filename, 'utf8')
  compileAndRun(chapter, code)
}

main()