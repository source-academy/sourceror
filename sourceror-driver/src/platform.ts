/**
 * This file contains the host platform library.
 * It defines what the host environment provides for FFI imports.
 */

/**
 * The main function to use.
 * Currently it takes no params, but eventually it should take params to specify the stream for output.
 */
export function makePlatformImports() {
  return {
    math: {
      sin: Math.sin,
      cos: Math.cos,
      tan: Math.tan,
      asin: Math.asin,
      acos: Math.acos,
      atan: Math.atan,
      atan2: Math.atan2,
      sinh: Math.sinh,
      cosh: Math.cosh,
      tanh: Math.tanh,
      asinh: Math.asinh,
      acosh: Math.acosh,
      atanh: Math.atanh,
    }
  }
}