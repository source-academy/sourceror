/**
 * This file contains the Decoder class, which decodes wasm handles into actually strings/booleans/etc.
 * This is needed because we can only transmit numbers directly through the FFI.
 */

export class Transcoder {
  mem: DataView;
  allocate_string: (len: number) => number;
  constructor() {}
  setMem(mem: DataView) {
    this.mem = mem;
  }
  setAllocateStringFunc(func: (len: number) => number) {
    this.allocate_string = func;
  }
  decodeString(handle: number): string {
    const len = this.mem.getUint32(handle, true);
    const decoder = new TextDecoder();
    return decoder.decode(new Uint8Array(this.mem.buffer, handle + 4, len));
  }
  /*
   * Note: You should not be holding onto any unregistered string handles when calling this function!
   * Otherwise the GC might reclaim those unregistered strings!
   * Returns a handle to the string in the memory.
   */
  encodeString(s: string): number {
    const encoder = new TextEncoder();
    const bytes = encoder.encode(s);
    const handle = this.allocate_string(bytes.length);
    // note: no need to set the length in the string, because allocateString() will already do it
    (new Uint8Array(this.mem.buffer, handle + 4, bytes.length)).set(bytes);
    return handle;
  }
}