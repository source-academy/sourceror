/**
 * This file contains the host platform library.
 * It defines what the host environment provides for FFI imports.
 */

import { Transcoder } from "./transcoder";

/**
 * The main function to use.
 * Takes the external context as param, that contains hooks for embedding-specific functions like display().
 */
export function makePlatformImports(externalContext: any, transcoder: Transcoder) {
  return {
    // MISC library
    misc: {
      get_time: () => new Date().getTime(),
      display: (text_handle: number) => {
        externalContext.display(transcoder.decodeString(text_handle));
      },
      parse_int: (text_handle: number, radix: number): number => {
        return parseInt(transcoder.decodeString(text_handle), radix);
      },
    },
    // MATH library
    math: {
      // trigonometric functions
      sin: Math.sin,
      cos: Math.cos,
      tan: Math.tan,
      asin: Math.asin,
      acos: Math.acos,
      atan: Math.atan,
      atan2: Math.atan2,
      // hyperbolic functions
      sinh: Math.sinh,
      cosh: Math.cosh,
      tanh: Math.tanh,
      asinh: Math.asinh,
      acosh: Math.acosh,
      atanh: Math.atanh,
      // exponent/logarithm functions
      sqrt: Math.sqrt,
      cbrt: Math.cbrt,
      exp: Math.exp,
      expm1: Math.expm1,
      log: Math.log,
      log1p: Math.log1p,
      log2: Math.log2,
      log10: Math.log10,
      pow: Math.pow,
      // hypot function
      hypot: Math.hypot,
      // rounding functions
      ceil: Math.ceil,
      floor: Math.floor,
      round: Math.round,
      trunc: Math.trunc,
      fround: Math.fround,
      // integer operations
      clz32: Math.clz32,
      imul: Math.imul,
      // random function
      random: Math.random,
    },
  };
}