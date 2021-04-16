import { Parser as AcornParser } from "acorn";

class SyntacticParser extends AcornParser {
  raiseRecoverable(pos, message) {
    if (message.includes("Identifier ") && message.includes(" has already been declared")) return;
    AcornParser.prototype.raiseRecoverable.call(this, pos, message);
  }
}

export function createImportOptions(err_handler) {
  return {
    sourceType: 'module',
    ecmaVersion: 6,
    locations: true,
    onInsertedSemicolon(end, loc) {
      err_handler();
    },
    onTrailingComma(_end, _loc) {
      err_handler();
    }
  }
}

export function parseCode(code) {
  let program;
  let has_errors = false;
  try {
    // we directly use acorn, because js-slang's parse is too restrictive
    // with attributes and exports
    program = SyntacticParser.parse(code, createImportOptions(() => { has_errors = true; }))
  } catch (error) {
    if (error instanceof SyntaxError) {
      has_errors = true;
    } else {
      throw error;
    }
  }
  if (program && !has_errors) return program;
  return undefined;
}
