/**
 * This file contains the Decoder class, which decodes wasm handles into actually strings/booleans/etc.
 * This is needed because we can only transmit numbers directly through the FFI.
 */

export class Transcoder {
  mem: DataView;
  constructor() {}
  setMem(mem: DataView) {
    this.mem = mem;
  }
  decodeString(handle: number): string {
    const len = this.mem.getUint32(handle, true);
    const decoder = new TextDecoder();
    return decoder.decode(new Uint8Array(this.mem.buffer, handle + 4, len));
  }
}